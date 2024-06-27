structure ConstantExtractor = struct

type print_name = string
type full_name = string
type accessor = string (*kind*) * print_name * full_name
type norm_def = string
type deps = string list

datatype constant = ConstDef of print_name * full_name * string * string list
                  | TypeDef of print_name * full_name * accessor list
                  | ConstAlias of print_name * print_name * full_name
                  | Overloading of full_name * full_name
                  | TypeAlias of print_name * print_name * full_name

type const_parser = (Proof.context -> constant list) parser

fun head_consts X =
  case Term.head_of X
    of Y as Const _ => [Y]
     | _ => []

fun s_expr ret bvs term =
  let val (head, args) = Term.strip_comb term
      val ret = if null args then ret else ")" :: ret
   in (
    case head
      of Const (name,_) => (name :: out_args ret bvs args)
       | Free (name, _) => (name :: out_args ret bvs args)
       | Var ((name,_), _) => (name :: out_args ret bvs args)
       | Abs (triple as (name,_,body)) =>
          if null args then ("\<lambda> " :: name :: ". " :: s_expr ret (name::bvs) body)
                       else (s_expr ret bvs (Abs triple) @ out_args ret bvs args)
       | Bound i => (nth bvs i :: out_args ret bvs args)
       | _ $ _ => error "impossible")
   |> (fn L => if null args then L else "(" :: L)
  end
and out_args ret bvs args = fold_rev (fn x => fn ret => (" " :: s_expr ret bvs x)) args ret

fun s_expression term = String.concat (s_expr [] [] term)

fun t_expr ret typ =
    case typ
      of Type (name, args) => (
            case args
              of [] => name::ret
               | [arg] => t_expr (name::ret) arg
               | arg :: args => "(" :: fold_rev (fn arg => fn ret => t_expr (","::ret) arg) args
                                                (t_expr (") "::name::ret) arg))
       | TFree (name, _) => (name :: ret)
       | TVar ((name,_), _) => (name :: ret)

fun t_expression typ = String.concat (t_expr [] typ)

fun get_def_const (Const(\<^const_name>\<open>Pure.conjunction\<close>, _) $ A $ B) = get_def_const A @ get_def_const B
  | get_def_const (Const("HOL.Trueprop", _) $ X) = get_def_const X
  | get_def_const (Const("HOL.eq", _) $ X $ _) = head_consts X
  | get_def_const (Const("HOL.iff", _) $ X $ _) = head_consts X
  | get_def_const (Const(\<^const_name>\<open>Pure.eq\<close>, _) $ X $ _) = get_def_const X
  | get_def_const (Const(\<^const_name>\<open>Pure.imp\<close>, _) $ _ $ X) = get_def_const X
  | get_def_const (Const(\<^const_name>\<open>Pure.all\<close>, _) $ Abs (_, _, X)) = get_def_const X
  | get_def_const _ = []

fun trim_term (Const("_type_constraint_", _) $ X) = trim_term X
  | trim_term (Const("_constraint_", _) $ X) = trim_term X
  | trim_term (Const("_constraint_", _) $ X $ _) = trim_term X
  | trim_term (X $ Y) = trim_term X $ trim_term Y
  | trim_term (Abs (N,T,X)) = Abs (N,T,trim_term X)
  | trim_term X = X

fun trim_makrup msg =
  let fun auto acc [] = []
        | auto acc (#"\005" :: L) = auto (not acc) L
        | auto true (x :: L) = x :: auto true L
        | auto false (_ :: L) = auto false L
   in String.implode (auto true (String.explode msg))
  end

fun norm_def ctxt def_prop =
  let val ctxt' = ctxt |> Proof_Context.set_mode Proof_Context.mode_default
                       |> Proof_Context.allow_dummies in
    case try (trim_makrup o Syntax.string_of_term ctxt o Syntax.read_prop ctxt') def_prop
      of SOME x => x
      | NONE => ""
  end

fun dep_const_names ctxt def_prop =
  let 
      val ctxt' = ctxt |> Proof_Context.set_mode Proof_Context.mode_default
                       |> Proof_Context.allow_dummies
      val term = trim_term (Syntax.read_prop ctxt' def_prop)
                 handle ERROR _ =>  Syntax.read_prop ctxt' "oops" (* don't know how to deal with it*)
   in map fst (Term.add_consts term []) end

fun get_def_const_names ctxt def_prop =
  let val consts = Syntax.parse_prop ctxt def_prop
                |> trim_term
                |> get_def_const
      val snames = map (trim_makrup o Syntax.string_of_term ctxt) consts
      val norm = norm_def ctxt def_prop
      val const_defs = map ConstDef (map (fn (a,b) => (a,b,norm,dep_const_names ctxt norm)) (snames ~~ map s_expression consts))
   in const_defs
  end

fun consts_by_names ctxt names =
  let val consts = map (trim_term o Syntax.parse_term ctxt) names
      val snames = map (trim_makrup o Syntax.string_of_term ctxt) consts
   in map ConstDef (map (fn (a,b) => (a,b,"",[])) (snames ~~ map s_expression consts))
  end

fun consts_by_bindings ctxt bindings =
  consts_by_names ctxt (map Binding.name_of bindings)

(* context / locale *)

fun context_wrapper (parser : const_parser) : const_parser =
  (Parse.opt_target -- parser) >> (fn (tgt, parse) => fn ctxt =>
    let val thy = Proof_Context.theory_of ctxt
        val ctxt' = case tgt of NONE => ctxt
                              | SOME tgt' => Target_Context.context_begin_named_cmd [] tgt' thy
     in parse ctxt'
    end )

(* definition *)

val def_parser : const_parser = Parse.command_name "definition" |-- context_wrapper (
  (Scan.option Parse_Spec.constdecl -- (Parse_Spec.opt_thm_name ":" -- Parse.prop) --
      Parse_Spec.if_assumes -- Parse.for_fixes) >> (fn (((_, (_, raw_spec)), _), _) => fn ctxt =>
        get_def_const_names ctxt raw_spec))


(* primrec *)

val rec_option_parser = Parse.group (K "option")
  (Plugin_Name.parse_filter >> K ()
   || Parse.reserved "nonexhaustive" >> K ()
   || Parse.reserved "transfer" >> K ());

val primrec_parser : const_parser = Parse.command_name "primrec" |-- context_wrapper (
  ((Scan.optional (\<^keyword>\<open>(\<close> |-- Parse.!!! (Parse.list1 rec_option_parser)
      --| \<^keyword>\<open>)\<close>) []) -- Parse_Spec.specification
    >> (fn (_, (_, specs)) => fn ctxt =>
      maps (fn ((_, spec),_,_) => get_def_const_names ctxt spec) specs )))

val where_alt_props_of_parser = Parse.where_ |-- Parse.!!! (Parse.enum1 "|"
  ((Parse.prop >> pair Binding.empty_atts) -- Scan.option (Parse.reserved "of" |-- Parse.const)));

val corec_option_parser = Parse.group (K "option")
  (Plugin_Name.parse_filter >> K ()
   || Parse.reserved "sequential" >> K ()
   || Parse.reserved "exhaustive" >> K ()
   || Parse.reserved "transfer" >> K ());

val premcorec_parser = ( Parse.command_name "primcorec"
                      || Parse.command_name "primcorecursive") |-- context_wrapper (
  ((Scan.optional (\<^keyword>\<open>(\<close> |-- Parse.!!! (Parse.list1 corec_option_parser)
      --| \<^keyword>\<open>)\<close>) []) --
    (Parse.vars -- where_alt_props_of_parser) >> (fn (_, (_, specs)) => fn ctxt =>
      maps (fn ((_, spec), _) => get_def_const_names ctxt spec) specs )))

(* fun *)

local

datatype function_config = FunctionConfig of
 {sequential: bool,
  default: string option,
  domintros: bool,
  partials: bool}

datatype function_opt =
    Sequential
  | Default of string
  | DomIntros
  | No_Partials

fun apply_opt Sequential (FunctionConfig {sequential = _, default, domintros, partials}) =
      FunctionConfig
        {sequential = true, default = default, domintros = domintros, partials = partials}
  | apply_opt (Default d) (FunctionConfig {sequential, default = _, domintros, partials}) =
      FunctionConfig
        {sequential = sequential, default = SOME d, domintros = domintros, partials = partials}
  | apply_opt DomIntros (FunctionConfig {sequential, default, domintros = _, partials}) =
      FunctionConfig
        {sequential = sequential, default = default, domintros = true, partials = partials}
  | apply_opt No_Partials (FunctionConfig {sequential, default, domintros, partials = _}) =
      FunctionConfig
        {sequential = sequential, default = default, domintros = domintros, partials = false}

val option_parser = Parse.group (fn () => "option")
    ((Parse.reserved "sequential" >> K Sequential)
     || ((Parse.reserved "default" |-- Parse.term) >> Default)
     || (Parse.reserved "domintros" >> K DomIntros)
     || (Parse.reserved "no_partials" >> K No_Partials))

fun config_parser default =
    (Scan.optional (\<^keyword>\<open>(\<close> |-- Parse.!!! (Parse.list1 option_parser) --| \<^keyword>\<open>)\<close>) [])
     >> (fn opts => fold apply_opt opts default)
in

fun function_parser default_cfg =
    config_parser default_cfg -- Parse_Spec.specification

val fun_config = FunctionConfig { sequential=true, default=NONE,
  domintros=false, partials=false }

end

val fun_parser : const_parser = ( Parse.command_name "fun"
                               || Parse.command_name "function" ) |-- context_wrapper (
  function_parser fun_config >> (
    fn (_, (_, (specs:Specification.multi_specs_cmd))) => fn ctxt =>
      maps (fn ((_, spec),_,_) => get_def_const_names ctxt spec) specs ))

(* lift_definition *)

val parse_param = Parse.name
val parse_params = Scan.optional (Args.parens (Parse.list parse_param)) [];

val lift_definition_parser = Parse.command_name "lift_definition" |-- context_wrapper (
  (parse_params --
      (((Parse.binding -- (\<^keyword>\<open>::\<close> |-- (Parse.typ >> SOME) -- Parse.opt_mixfix')
          >> Scan.triple2) --
        (\<^keyword>\<open>is\<close> |-- Parse.term) --
        Scan.optional (Parse.$$$ "parametric" |-- Parse.!!! Parse.thms1) []) >> Scan.triple1)
     >> (fn (_, ((b,_,_), _, _)) => fn ctxt => consts_by_bindings ctxt [b])))

(* specification *)

val opt_name = Scan.optional (Parse.name --| \<^keyword>\<open>:\<close>) ""
val opt_overloaded = Parse.opt_keyword "overloaded"

val specification_parser = Parse.command_name "specification" |-- context_wrapper (
     (\<^keyword>\<open>(\<close> |-- Scan.repeat1 (opt_name -- Parse.term -- opt_overloaded) --| \<^keyword>\<open>)\<close> --
      Scan.repeat1 ((Parse_Spec.opt_thm_name ":" >> apfst Binding.name_of) -- Parse.prop)
      >> (fn (cos, props) => fn ctxt =>
        let val norm = map (norm_def ctxt o #2) props
                    |> String.concatWith "\n"
         in map (fn ((_, raw_term), _) =>
            let val const = trim_term (Syntax.parse_term ctxt raw_term)
                val sname = trim_makrup (Syntax.string_of_term ctxt const)
             in ConstDef (sname, s_expression const, norm, dep_const_names ctxt norm)
            end ) cos
        end )))

(* datatype *)

fun accessor ctxt kind term =
  (kind, trim_makrup (Syntax.string_of_term ctxt term), s_expression term)

fun accessor_s ctxt kind str =
  let val term = Syntax.parse_term ctxt str
              |> trim_term
   in accessor ctxt kind term
  end

(* hacks *)
(* these hacks make the extractor work with Pure instead of Main *)

val hack_accessors_of_ctr : (Proof.context -> string -> (string * string * string) list)
                            option Unsynchronized.ref = Unsynchronized.ref NONE
val hack_accessors_of_ctr_locker : unit Synchronized.var = Synchronized.var "_hack_accessors_of_ctr" ()
val lex_read_accessors_of_ctr = ML_Lex.read "\
\let\n\
\fun accessors_of_ctr ctxt tname =\n\
  \case Ctr_Sugar.ctr_sugar_of ctxt tname\n\
  \of SOME ctr => [ConstantExtractor.accessor ctxt \"case_split\" (#casex ctr)]\n\
                \@ map (ConstantExtractor.accessor ctxt \"constructor\") (#ctrs ctr)\n\
                \@ map (ConstantExtractor.accessor ctxt \"discs\") (#discs ctr) (*something I don't know, looks strange?*)\n\
                \@ maps (map (ConstantExtractor.accessor ctxt \"selector\")) (#selss ctr)\n\
     \| NONE => []\n\
\in\n\
  \ConstantExtractor.hack_accessors_of_ctr := SOME accessors_of_ctr\n\
\end"

val hack_datatype_parser : (Token.T list -> (Proof.context -> constant list) * Token.T list)
                           option Unsynchronized.ref  = Unsynchronized.ref NONE
val hack_datatype_parser_locker : unit Synchronized.var = Synchronized.var "_hack_datatype_parser" ()
val lex_read_datatype_parser = ML_Lex.read "\
\let\n\
\fun accessors_of_ctr ctxt tname =\n\
  \case Ctr_Sugar.ctr_sugar_of ctxt tname\n\
  \of SOME ctr => [ConstantExtractor.accessor ctxt \"case_split\" (#casex ctr)]\n\
                \@ map (ConstantExtractor.accessor ctxt \"constructor\") (#ctrs ctr)\n\
                \@ map (ConstantExtractor.accessor ctxt \"discs\") (#discs ctr) (*something I don't know, looks strange?*)\n\
                \@ maps (map (ConstantExtractor.accessor ctxt \"selector\")) (#selss ctr)\n\
     \| NONE => []\n\
\val datatype_parser = ConstantExtractor.context_wrapper (\n\
  \BNF_FP_Def_Sugar.parse_co_datatype >> (fn (_, types) => fn ctxt =>\n\
    \maps (fn (((((_, b), _), _), _), _) =>\n\
      \let val thy = Proof_Context.theory_of ctxt\n\
          \val sname = Binding.name_of b\n\
          \val tname = Sign.intern_type thy sname\n\
       \in [ConstantExtractor.TypeDef (sname, tname,\n\
            \accessors_of_ctr ctxt tname @\n\
            \(case BNF_Def.bnf_of ctxt tname\n\
               \of SOME bnf => [\n\
                      \ConstantExtractor.accessor ctxt \"mapper\" (BNF_Def.map_of_bnf bnf),\n\
                      \ConstantExtractor.accessor ctxt \"predicator\" (BNF_Def.pred_of_bnf bnf),\n\
                      \ConstantExtractor.accessor ctxt \"relator\" (BNF_Def.rel_of_bnf bnf) ] @\n\
                      \map (ConstantExtractor.accessor ctxt \"sets\") (BNF_Def.sets_of_bnf bnf)\n\
                \| NONE => [])\n\
          \)]\n\
      \end) types))\n\
\in\n\
  \ConstantExtractor.hack_datatype_parser := SOME datatype_parser\n\
\end"

val hack_record_parser : (Token.T list -> (Proof.context -> constant list) * Token.T list)
                           option Unsynchronized.ref  = Unsynchronized.ref NONE
val hack_record_parser_locker : unit Synchronized.var = Synchronized.var "_hack_record_parser" ()
val lex_read_record_parser = ML_Lex.read "\
\let\n\
\fun accessors_of_ctr ctxt tname =\n\
  \case Ctr_Sugar.ctr_sugar_of ctxt tname\n\
  \of SOME ctr => [ConstantExtractor.accessor ctxt \"case_split\" (#casex ctr)]\n\
                \@ map (ConstantExtractor.accessor ctxt \"constructor\") (#ctrs ctr)\n\
                \@ map (ConstantExtractor.accessor ctxt \"discs\") (#discs ctr) (*something I don't know, looks strange?*)\n\
                \@ maps (map (ConstantExtractor.accessor ctxt \"selector\")) (#selss ctr)\n\
     \| NONE => []\n\
\val record_parser = ConstantExtractor.context_wrapper (\n\
  \(Parse_Spec.overloaded -- (Parse.type_args_constrained -- Parse.binding) --\n\
      \(\<^keyword>\<open>=\<close> |-- Scan.option (Parse.typ --| \<^keyword>\<open>+\<close>) --\n\
        \Scan.repeat1 Parse.const_binding)\n\
  \>> (fn ((_,(_,b)), _) => fn ctxt =>\n\
    \let val thy = Proof_Context.theory_of ctxt\n\
        \val sname = Binding.name_of b\n\
        \val tname = Sign.intern_type thy sname\n\
    \in [ConstantExtractor.TypeDef (sname, tname,\n\
          \accessors_of_ctr ctxt tname @\n\
          \(case Record.get_info thy tname\n\
             \of NONE => []\n\
              \| SOME info =>\n\
                  \maps (fn (name, _) =>\n\
                      \[ConstantExtractor.accessor_s ctxt \"getter\" name,\n\
                       \ConstantExtractor.accessor_s ctxt \"setter\" (name ^ Record.updateN)]\n\
                    \) (#fields info))\n\
        \),\n\
        \ConstantExtractor.TypeAlias (sname, sname ^ Record.ext_typeN, tname ^ Record.ext_typeN)]\n\
   \end)))\n\
\in\n\
  \ConstantExtractor.hack_record_parser := SOME record_parser\n\
\end"


val hack_typedef_impl : (binding -> Proof.context -> constant list)
                           option Unsynchronized.ref  = Unsynchronized.ref NONE
val hack_typedef_impl_locker : unit Synchronized.var = Synchronized.var "_hack_typedef_impl" ()
val lex_read_typedef_impl = ML_Lex.read "\
\let\n\
\fun accessors_of_ctr ctxt tname =\n\
  \case Ctr_Sugar.ctr_sugar_of ctxt tname\n\
  \of SOME ctr => [ConstantExtractor.accessor ctxt \"case_split\" (#casex ctr)]\n\
                \@ map (ConstantExtractor.accessor ctxt \"constructor\") (#ctrs ctr)\n\
                \@ map (ConstantExtractor.accessor ctxt \"discs\") (#discs ctr) (*something I don't know, looks strange?*)\n\
                \@ maps (map (ConstantExtractor.accessor ctxt \"selector\")) (#selss ctr)\n\
     \| NONE => []\n\
\fun typedef_impl b = fn ctxt =>\n\
     \let val thy = Proof_Context.theory_of ctxt\n\
        \val sname = Binding.name_of b\n\
        \val tname = Sign.intern_type thy sname\n\
    \in [ConstantExtractor.TypeDef (sname, tname,\n\
          \accessors_of_ctr ctxt tname @\n\
          \maps (fn (info,_) =>\n\
              \[ConstantExtractor.accessor_s ctxt \"map-to-abstraction\" (#Abs_name info),\n\
               \ConstantExtractor.accessor_s ctxt \"representation\" (#Rep_name info)]\n\
            \) (Typedef.get_info ctxt tname)\n\
       \)]\n\
   \end\n\
\in\n\
  \ConstantExtractor.hack_typedef_impl := SOME typedef_impl\n\
\end"

fun get_hacked hack hack_locker lex_read ctxt =
  case !hack
    of SOME ret => ret
     | NONE =>
  Synchronized.change_result hack_locker (fn () =>
    case !hack
      of SOME ret => (ret, ())
       | NONE => let
          val _ = ML_Context.expression Position.none lex_read (Context.Proof ctxt)
      in (the (!hack), ())
     end)

fun accessors_of_ctr ctxt =
  get_hacked hack_accessors_of_ctr hack_accessors_of_ctr_locker lex_read_accessors_of_ctr ctxt ctxt

fun get_datatype_parser ctxt =
  get_hacked hack_datatype_parser hack_datatype_parser_locker lex_read_datatype_parser ctxt

fun get_record_parser ctxt =
  get_hacked hack_record_parser hack_record_parser_locker lex_read_record_parser ctxt

fun typedef_impl b ctxt =
  get_hacked hack_typedef_impl hack_typedef_impl_locker lex_read_typedef_impl ctxt b ctxt

(* datatype *)

val datatype_parser : const_parser = (Parse.command_name "datatype" || Parse.command_name "codatatype") |-- (fn toks =>
  ((fn ctxt => let val parser = get_datatype_parser ctxt
     in #1 (parser toks) ctxt
    end ),
   [Token.eof]))

(* quotient_definition *)

val quotient_definition_parser = Parse.command_name "quotient_definition" |-- context_wrapper (
  (Scan.option Parse_Spec.constdecl --
        Parse.!!! (Parse_Spec.opt_thm_name ":" -- (Parse.term -- (\<^keyword>\<open>is\<close> |-- Parse.term)))
   >> (fn (_, (_, (x,_))) => fn ctxt => consts_by_names ctxt [x])))

val quotient_type_parser = Parse.command_name "quotient_type" |-- context_wrapper (
  (Parse_Spec.overloaded -- (Parse.type_args -- Parse.binding --
        Parse.opt_mixfix -- (\<^keyword>\<open>=\<close> |-- Parse.typ) -- (Parse.$$$ "/" |--
          Scan.optional (Parse.reserved "partial" -- \<^keyword>\<open>:\<close> >> K true) false -- Parse.term) --
        Scan.option (Parse.$$$ "morphisms" |-- Parse.!!! (Parse.binding -- Parse.binding)) --
        Scan.option (Parse.$$$ "parametric" |-- Parse.!!! Parse.thm))
   >> (fn (_, ((((((_,b),_),_),_),_),_)) => fn ctxt =>
     let val thy = Proof_Context.theory_of ctxt
          val sname = Binding.name_of b
          val tname = Sign.intern_type thy sname
      in [TypeDef (sname, tname,
            accessors_of_ctr ctxt tname
          )]
     end )))

(* record *)

val record_parser : const_parser = Parse.command_name "record" |-- (fn toks =>
  ((fn ctxt => let val parser = get_record_parser ctxt
     in #1 (parser toks) ctxt
    end ),
   [Token.eof]))

(* axiomatization *)

val axiomatization =
  Parse.and_list1 (Parse_Spec.thm_name ":" -- Parse.prop) --
  Parse_Spec.if_assumes -- Parse.for_fixes >> (fn ((a, b), c) => (c, b, a));

val axiom_parser = Parse.command_name "axiomatization" |-- context_wrapper (
  (Scan.optional Parse.vars [] --
      Scan.optional (Parse.where_ |-- Parse.!!! axiomatization) ([], [], [])
      >> (fn (a, _) => fn ctxt => consts_by_bindings ctxt (map #1 a) )))

(* alias *)

val alias_parser = Parse.command_name "alias" |-- context_wrapper (
  (Parse.binding -- (Parse.!!! \<^keyword>\<open>=\<close> |-- Parse.name_position)
      >> (fn (name, (term, _)) => fn ctxt =>
    let val iname = s_expression (trim_term (Syntax.parse_term ctxt term))
     in [ConstAlias (Binding.name_of name, term, iname)]
    end )))

(* adhoc_overloading *)

val adhoc_overloading_parser = Parse.command_name "adhoc_overloading" |-- context_wrapper (
  (Parse.and_list1 (Parse.const -- Scan.repeat Parse.term) >> (fn oprs => fn ctxt =>
    let val thy = Proof_Context.theory_of ctxt
     in maps (fn (const, ovls) => map (fn ovl =>
        let val const = Sign.intern_const thy const
            val ovl = Sign.intern_const thy ovl
         in Overloading (const, ovl)
        end
      ) ovls) oprs
    end)))

(* class *)

val class_context_elements =
  Scan.repeat1 Parse_Spec.context_element;

val class_val =
  ((Parse_Spec.class_expression -- Scan.optional Parse_Spec.opening [])
    || Parse_Spec.opening >> pair [])
  -- Scan.optional (\<^keyword>\<open>+\<close> |-- Parse.!!! class_context_elements) [] ||
  class_context_elements >> pair ([], []);

val class_parser = Parse.command_name "class" |-- context_wrapper (
  (Parse.binding -- Scan.optional (\<^keyword>\<open>=\<close> |-- class_val) (([], []), []) -- Parse.opt_begin
    >> (fn ((_, (_, elems)), _) => fn ctxt =>
      let val bindings = maps (fn Element.Fixes bs => map #1 bs
                                | _ => []) elems
       in consts_by_bindings ctxt bindings
      end )))


(* inductive *)

val inductive_parser = ( Parse.command_name "inductive"
                      || Parse.command_name "coinductive"
                      || Parse.command_name "inductive_set"
                      || Parse.command_name "coinductive_set") |-- context_wrapper (
  Parse.vars -- Parse.for_fixes --
  Scan.optional Parse_Spec.where_multi_specs [] --
  Scan.optional (Parse.$$$ "monos" |-- Parse.!!! Parse.thms1) []
  >> (fn (((bs,_),_),_) => fn ctxt => consts_by_bindings ctxt (map #1 bs))
  )

(* type_alias *)

val type_alias_parser = Parse.command_name "type_alias" |-- context_wrapper
  (Parse.binding -- (Parse.!!! \<^keyword>\<open>=\<close> |-- Parse.typ)
      >> (fn (b,t) => fn ctxt =>
        let val typ = Syntax.parse_typ ctxt t
            val sname = Syntax.string_of_typ ctxt typ
         in [TypeAlias (Binding.name_of b, sname, t_expression typ)]
        end ))

val type_synonym_parser = Parse.command_name "type_synonym" |-- context_wrapper
  (Parse.type_args -- Parse.binding --
      (\<^keyword>\<open>=\<close> |-- Parse.!!! (Parse.typ -- Parse.opt_mixfix'))
      >> (fn ((_,b),(t,_)) => fn ctxt =>
        let val typ = Syntax.parse_typ ctxt t
            val sname = Syntax.string_of_typ ctxt typ
         in [TypeAlias (Binding.name_of b, sname, t_expression typ)]
        end ))

(* typedecl *)

val typedecl_parser = Parse.command_name "typedecl" |-- context_wrapper
  (Parse.type_args -- Parse.binding -- Parse.opt_mixfix
      >> (fn ((_, b), _) => fn ctxt =>
    let val thy = Proof_Context.theory_of ctxt
        val sname = Binding.name_of b
        val tname = Sign.intern_type thy sname
    in [TypeDef (sname, tname, [])]
   end));

(* typedef *)

val typedef_parser = Parse.command_name "typedef" |-- context_wrapper
  (Parse_Spec.overloaded -- Parse.type_args_constrained -- Parse.binding -- Parse.opt_mixfix --
      (\<^keyword>\<open>=\<close> |-- Parse.term) --
      Scan.option (Parse.$$$ "morphisms" |-- Parse.!!! (Parse.binding -- Parse.binding))
   >> (fn ((((_,b),_),_), _) => typedef_impl b));

(** Trim **)

fun trim_makrup msg =
  let fun auto _ [] = []
        | auto acc (#"\005" :: L) = auto (not acc) L
        | auto true (x :: L) = x :: auto true L
        | auto false (_ :: L) = auto false L
   in String.implode (auto true (String.explode msg))
  end

fun trim_defele (ConstDef (a,b,nm,deps)) = ConstDef (trim_makrup a, b, nm, deps)
  | trim_defele (TypeDef (a,b,l)) =
      TypeDef (trim_makrup a, b, map (fn (k,p,f) => (k,trim_makrup p,f)) l)
  | trim_defele (ConstAlias (a,b,c)) = ConstAlias (trim_makrup a, trim_makrup b, c)
  | trim_defele (TypeAlias (a,b,c)) = TypeAlias (trim_makrup a, trim_makrup b, c)
  | trim_defele (Overloading (f1,f2)) = Overloading (f1,f2)

(** Assemble All **)

val parser = def_parser
          || fun_parser
          || primrec_parser
          || axiom_parser
          || alias_parser
          || class_parser
          || datatype_parser
          || inductive_parser
          || adhoc_overloading_parser
          || premcorec_parser
          || lift_definition_parser
          || quotient_definition_parser
          || quotient_type_parser
          || record_parser
          || specification_parser
          || type_alias_parser
          || typedecl_parser
          || typedef_parser
          || type_synonym_parser

val _ = Printer.show_markup_default := false

fun parse' parser ctxt =
  let val ctxt = ctxt
              |> Config.put Printer.show_types false
              |> Config.put Printer.show_sorts false
              |> Config.put Printer.show_markup false
              |> Config.put Printer.show_question_marks false
      val keywords = Thy_Header.get_keywords' ctxt
   in fn str => Parse.read_embedded ctxt keywords parser (Input.string str) ctxt
             |> map trim_defele
  end

val parse = parse' parser

fun scan_succeed _ = (K [], [Token.eof])

fun safe_parse (st, cmd) = let val ctxt = Toplevel.context_of st
  in parse' (parser || scan_succeed) ctxt cmd end

fun safe_parse' (ctxt, cmd) = parse' (parser || scan_succeed) ctxt cmd

end

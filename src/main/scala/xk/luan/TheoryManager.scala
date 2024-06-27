package xk.luan

import de.unruh.isabelle.control.{Isabelle, OperationCollection}
import de.unruh.isabelle.mlvalue.Implicits._
import de.unruh.isabelle.mlvalue.MLValue.{compileFunction, compileFunction0}
import de.unruh.isabelle.mlvalue.Version
import de.unruh.isabelle.pure.Implicits._
import de.unruh.isabelle.pure._
import xk.luan.TheoryManager.Ops

import java.nio.file.{Path, Paths}
import scala.math.max

/* we need:
 * 1. parsing the thy file
 * 2. get imported theories
 * 3. (maybe) get all constants in file
 * 4. (maybe) get all lemma/definitions in file
 *  */

class TheoryManager(val sessionName: String)(implicit isabelle: Isabelle) {
  def findTheoryFile(thyName: String): Option[Path] =
    Ops.find_theory_file(thyName).force.retrieveNow

  def parseTheory(thy: Theory, thyText: String, ignore: Boolean = false): List[(String, String, (Int, Int, Int))] = {
    val transWithCode = Transition.parseOuterSyntax(thy, thyText)
    val filteredResults = if (ignore) transWithCode.filterNot(_._1.isIgnored) else transWithCode
    filteredResults.map { case (tr, code) =>
      val pos = tr.position
      val posTuple = (pos.line.get, pos.offset.get, pos.endOffset.get)
      (tr.name, code, posTuple)
    }
  }

  def localesOpenedForState(toplevelState: ToplevelState): List[String] =
    Ops.locales_opened_for_state(toplevelState).force.retrieveNow

  def getDependentTheorems(toplevelState: ToplevelState, theoremName: String): List[(String, Int)] =
    Ops.get_dependent_thms(toplevelState, theoremName).force.retrieveNow

  def getGlobalFacts(theory: Theory): List[String] =
    Ops.global_facts_and_defs(theory).force.retrieveNow

  def getCommands(thy: Theory, thyText: String): List[(Int, String)] = {
    val lineNumAndCmd = Ops.get_commands(thy, thyText).force.retrieveNow
    lineNumAndCmd.map({ case (lineNum, cmd) => (lineNum.getOrElse(0), cmd) })
  }

  def getTheoryImports(thyText: String): List[String] = {
    val header = TheoryHeader.read(thyText)
    header.imports.map(_.stripSuffix("\"").stripSuffix("\""))
  }

  def getTheoryImports(commands: List[String]): List[String] =
    commands.find(_.startsWith("theory")) match {
      case Some(header) =>
        val imports = header.split("imports").last.split("begin").head
        imports.trim.split("\\s+").map(_.stripSuffix("\"").stripSuffix("\"")).toList
      case None => List()
    }

  /** An imported theory may be in one of the following forms:
    *
    * (1) X.Y where X is a session name and Y is the theory name. (2) Y where Y is the theory name. (3) relative path to
    * the current theory file. (4) "~~/src/..." path
    *
    * We treat the first three cases all as a file path (even though X.Y is not a path). First, we get the normalized
    * path of the imported theory relative to the current theory file. Then, we check if the path is in
    * `sessionFilesMap(sessionName)`. If so, the imported theory must be in second or the third form, and we can import
    * it by using `${sessionName}.Y`. Otherwise, the imported theory may be in the first form so that we can import it
    * as it is. It could also be in the second or the third form but not in `sessionFilesMap(sessionName)`, this
    * indicates that this theory is not correctly imported.
    *
    * For the forth case, there is only one well-formatted theory file used this import in AFP. So our solution will be
    * very specific to it. Such usage should be avoided in practice.
    */
  private def getTheorySource(name: String, sessionFilesMap: Map[String, List[os.Path]]): String = {
    val sanitisedName = name.stripPrefix("\"").stripSuffix("\"")
    // 1. deal with corner case (only one case in AFP)
    if (sanitisedName.startsWith("~~/src/")) {
      if (!sanitisedName.startsWith("~~/src/HOL/Library/")) throw new Exception(s"Unsupported import $name")
      return s"HOL-Library.${sanitisedName.stripPrefix("~~/src/HOL/Library/")}"
    }
    // 2. deal with path case and X.Y case
    val thyName = sanitisedName.split("/").last
    val sessionFiles = sessionFilesMap(sessionName)
    if (sessionFiles.exists(_.last == s"$thyName.thy"))
      s"$sessionName.$thyName"
    else sanitisedName
  }

  def initToplevel(): ToplevelState = Ops.init_toplevel().force.retrieveNow

  def beginTheory(text: String, path: os.Path, sessionFilesMap: Map[String, List[os.Path]]): Theory = {
    val header = TheoryHeader.read(text)
    val masterDir = Option(path.toNIO.getParent).getOrElse(Paths.get(""))
    Ops
      .begin_theory(
        masterDir,
        header,
        header.imports.map(header => getTheorySource(header, sessionFilesMap)).map(Theory.apply)
      )
      .force
      .retrieveNow
  }
  def getThms(toplevelState: ToplevelState, thmNames: List[(String, Int)]): List[String] = {
    // for (name, 0), the index is still 0; for (name, n) where n > 0, the index should be `n-1`
    val thmsList = thmNames.map(nameIdx => getThmWithIndex(toplevelState, nameIdx._1, max(0, nameIdx._2 - 1)))
    val thmsString = Ops.get_thms(toplevelState, thmsList).force.retrieveNow
    thmsString.split("\n").toList.map(_.strip)
  }

  private def getThmWithIndex(toplevelState: ToplevelState, name: String, index: Int): Thm = {
    Ops.get_thm_with_index(toplevelState, name, index).force.retrieveNow
  }

  def thmNameString(name: String, index: Int): String =
    index match {
      case 0 => name
      case n => f"$name(${n - 1})"
    }

  def getLocaleDependencies(theory: Theory): List[(String, String, List[(String, Boolean)], Position)] =
    Ops.get_local_dependencies(theory).force.retrieveNow
}

object TheoryManager extends OperationCollection {
  /* 1. Toplevel.make_state
   *  2. Toplevel.init_toplevel
   *  3. Resources.begin_theory
   *  4. Position.line_of
   *  5. Toplevel.pos_of
   *  6. Toplevel.name_of
   *  7. Outer_Syntax.parse_text
   *  8. Position.start
   *  9. Toplevel.theory_of
   *  10. Global_Theory.get_thms
   *  11. Thm_Deps.thm_deps
   *  12. Global_Theory.all_thms_of
   *  13. YXML.content_of
   *  14. Pretty.string_of
   *  15. Pretty.chunks
   *  16. Isar_Cmd.pretty_theorems
   *  17. Toplevel.context_of
   *  18. Proof_Context.pretty_fact
   *  19. Resources.find_theory_file
   *  20. Proof_Context.get_thms
   * */

  // noinspection TypeAnnotation
  protected final class Ops(implicit isabelle: Isabelle) {
    val init_toplevel = compileFunction0[ToplevelState](
      if (Version.from2023) "fn () => Toplevel.make_state NONE" else "Toplevel.init_toplevel"
    )

    val begin_theory = compileFunction[Path, TheoryHeader, List[Theory], Theory](
      "fn (path, header, parents) => Resources.begin_theory path header parents"
    )

    val get_commands = compileFunction[Theory, String, List[(Option[Int], String)]]("""fn (thy, text) =>
        |  map (fn tr => ((Position.line_of o Toplevel.pos_of) tr, Toplevel.name_of tr))
        |      (Outer_Syntax.parse_text thy (K thy) Position.start text)
        |  |> filter (fn (_, name) => not (name = "<ignored>"))""".stripMargin)

    val locales_opened_for_state =
      compileFunction[ToplevelState, List[String]]("fn (tls) => Locale.get_locales (Toplevel.theory_of tls)")

    val get_dependent_thms = compileFunction[ToplevelState, String, List[(String, Int)]]("""fn (tls, name) =>
        | let val thy = Toplevel.theory_of tls;
        |     val thm = Global_Theory.get_thms thy name;
        | in
        |     map (fn x => (#2 x)) (Thm_Deps.thm_deps thy thm)
        | end""".stripMargin)

    val global_facts_and_defs =
      compileFunction[Theory, List[String]]("fn thy => Global_Theory.all_thms_of thy false |> map #1")

    val pretty_theorems = compileFunction[ToplevelState, String](
      "fn tls => (YXML.content_of o Pretty.string_of o Pretty.chunks o (Isar_Cmd.pretty_theorems false)) tls"
    )

    val get_thms = compileFunction[ToplevelState, List[Thm], String]("""fn (st, thms) => let
        |  val ctxt = Toplevel.context_of st
        |in (YXML.content_of o Pretty.string_of o (Proof_Context.pretty_fact ctxt)) ("", thms) end""".stripMargin)

    val get_thm_with_index = compileFunction[ToplevelState, String, Int, Thm](
      """fn (tls, name, idx) => List.nth (Proof_Context.get_thms (Toplevel.context_of tls) name, idx)"""
    )

    val find_theory_file =
      compileFunction[String, Option[Path]]("""fn thy_name => Resources.find_theory_file thy_name""")

    val get_local_dependencies = compileFunction[Theory, List[(String, String, List[(String, Boolean)], Position)]](
      """fn thy => Locale.dest_dependencies [] thy |> map (fn dep =>
        | (#source dep, #target dep, #prefix dep, #pos dep)
        |)""".stripMargin
    )
  }

  override protected def newOps(implicit isabelle: Isabelle) =
    new this.Ops

  def filterParsedText(text: List[String]): List[String] = {
    text.filterNot({ line =>
      line.isEmpty || (line.startsWith("(*") && line.endsWith("*)"))
    })
  }
}

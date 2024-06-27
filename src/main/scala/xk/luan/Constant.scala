package xk.luan

import upickle.default.{ReadWriter => RW, macroRW}
import de.unruh.isabelle.control.Isabelle.{DList, DString, executionContext}
import de.unruh.isabelle.control.{Isabelle, OperationCollection}
import de.unruh.isabelle.mlvalue.MLValue.matchFailExn
import de.unruh.isabelle.mlvalue.{MLRetrieveFunction, MLStoreFunction, MLValue}
import xk.luan.Constant.Ops

import scala.concurrent.Future

case class Accessor(kind: String, printName: String, fullName: String) {
  override def toString: String = s"Accessor(kind=$kind, printName=$printName, fullName=$fullName)"
}

sealed abstract class Constant

case class ConstDef(printName: String, fullName: String, normDef: String, deps: List[String]) extends Constant {
  override def toString: String = {
    val depsString = deps.mkString("[", ", ", "]")
    s"ConstDef(printName=$printName, fullName=$fullName, normDef=$normDef, deps=$depsString)"
  }
}

case class TypeDef(printName: String, fullName: String, accessors: List[Accessor]) extends Constant {
  override def toString: String = {
    val accessorsString = accessors.mkString("[", ", ", "]")
    s"TypeDef(printName=$printName, fullName=$fullName, accessors=$accessorsString)"
  }
}

case class ConstAlias(abbreviation: String, originName: String, originFullName: String) extends Constant {
  override def toString: String =
    s"ConstAlias(abbreviation=$abbreviation, originName=$originName, originFullName=$originFullName)"
}

case class Overloading(overloaded: String, instance: String) extends Constant {
  override def toString: String = s"Overloading(overloaded=$overloaded, instance=$instance)"
}

case class TypeAlias(abbreviation: String, originName: String, originFullName: String) extends Constant {
  override def toString: String =
    s"TypeAlias(abbreviation=$abbreviation, originName=$originName, originFullName=$originFullName)"
}

object Accessor {
  implicit val rw: RW[Accessor] = macroRW
}

object Constant extends OperationCollection {
  implicit val rw: RW[Constant] = RW.merge(ConstDef.rw, TypeDef.rw, ConstAlias.rw, Overloading.rw, TypeAlias.rw)
  implicit val constantConverter: MLValue.Converter[Constant] =
    ConstantConverter

  override protected def newOps(implicit isabelle: Isabelle) =
    new this.Ops

  protected final class Ops(implicit isabelle: Isabelle) {
    isabelle.executeMLCodeNow("exception E_Constant of ConstantExtractorIsaUtils.constant")

    lazy val retrieveConstant: MLRetrieveFunction[Constant] =
      MLRetrieveFunction[Constant]("""
        |fn ConstantExtractorIsaUtils.ConstDef (a,b,c,d) => DList [DString "ConstDef", DString a, DString b, DString c, DList (map DString d)]
        |  | ConstantExtractorIsaUtils.TypeDef (a,b,c) => DList [DString "TypeDef", DString a, DString b,
        |                              DList (map(fn (a1,b1,c1) => DList [DString a1, DString b1, DString c1]) c)]
        |  | ConstantExtractorIsaUtils.ConstAlias (a,b,c) => DList [DString "ConstAlias", DString a, DString b, DString c]
        |  | ConstantExtractorIsaUtils.Overloading (a,b) => DList [DString "Overloading", DString a, DString b]
        |  | ConstantExtractorIsaUtils.TypeAlias (a,b,c) => DList [DString "TypeAlias", DString a, DString b, DString c]
        |""".stripMargin)(isabelle, ConstantConverter)
    lazy val storeConstant: MLStoreFunction[Constant] = MLStoreFunction[Constant]("""
        |fn DList [DString "ConstDef", DString a, DString b, DString c, DList d] =>
        |     ConstantExtractorIsaUtils.ConstDef (a,b,c,map (fn DString a1 => a1) d)
        |  | DList [DString "TypeDef", DString a, DString b, DList c] =>
        |     ConstantExtractorIsaUtils.TypeDef (a,b,map (fn DList [DString a1, DString b1, DString c1] => (a1,b1,c1)) c)
        |  | DList [DString "ConstAlias", DString a, DString b, DString c] =>
        |     ConstantExtractorIsaUtils.ConstAlias (a,b,c)
        |  | DList [DString "Overloading", DString a, DString b] =>
        |     ConstantExtractorIsaUtils.Overloading (a,b)
        |  | DList [DString "TypeAlias", DString a, DString b, DString c] =>
        |     ConstantExtractorIsaUtils.TypeAlias (a,b,c)
        |""".stripMargin)(isabelle, ConstantConverter)
  }
}

object ConstDef {
  implicit val rw: RW[ConstDef] = macroRW
}

object TypeDef {
  implicit val rw: RW[TypeDef] = macroRW
}

object ConstAlias {
  implicit val rw: RW[ConstAlias] = macroRW
}

object Overloading {
  implicit val rw: RW[Overloading] = macroRW
}

object TypeAlias {
  implicit val rw: RW[TypeAlias] = macroRW
}

object ConstantConverter extends MLValue.Converter[Constant] {
  override def mlType(implicit isabelle: Isabelle): String = "ConstantExtractorIsaUtils.constant"
  @inline override def valueToExn(implicit isabelle: Isabelle): String =
    "E_Constant"
  @inline override def exnToValue(implicit isabelle: Isabelle): String =
    s"fn (E_Constant x) => x | ${matchFailExn("ConstantConverter.exnToValue")}"

  override def retrieve(value: MLValue[Constant])(implicit isabelle: Isabelle): Future[Constant] = {
    for (DList(DString(name), data @ _*) <- Ops.retrieveConstant(value))
      yield name match {
        case "ConstDef" =>
          val (printName, fullName, normDef, deps) = data.toList match {
            case List(DString(pn), DString(fn), DString(nd), DList(dep @ _*)) => (pn, fn, nd, dep)
            case _ => throw new IllegalArgumentException(s"Unexpected ConstDef format ${data.toList}")
          }
          val depsList = deps.map {
            case DString(a) => a
            case _          => throw new IllegalArgumentException(s"Unexpected deps format $deps")
          }.toList
          ConstDef(printName, fullName, normDef, depsList)
        case "TypeDef" =>
          val (printName, fullName, accessors) = data.toList match {
            case List(DString(pn), DString(fn), DList(acc @ _*)) => (pn, fn, acc)
            case _ => throw new IllegalArgumentException(s"Unexpected TypeDef format ${data.toList}")
          }
          val accessorsList = accessors.map {
            case DList(DString(a), DString(b), DString(c)) => Accessor(a, b, c)
            case _ => throw new IllegalArgumentException(s"Unexpected accessors format $accessors")
          }.toList
          TypeDef(printName, fullName, accessorsList)
        case "ConstAlias" =>
          val (abbreviation, originName, originFullName) = data.toList match {
            case List(DString(a), DString(b), DString(c)) => (a, b, c)
            case _ => throw new IllegalArgumentException(s"Unexpected ConstAlias format ${data.toList}")
          }
          ConstAlias(abbreviation, originName, originFullName)
        case "Overloading" =>
          val (overloaded, instance) = data.toList match {
            case List(DString(a), DString(b)) => (a, b)
            case _ => throw new IllegalArgumentException(s"Unexpected Overloading format ${data.toList}")
          }
          Overloading(overloaded, instance)
        case "TypeAlias" =>
          val (abbreviation, originName, originFullName) = data.toList match {
            case List(DString(a), DString(b), DString(c)) => (a, b, c)
            case _ => throw new IllegalArgumentException(s"Unexpected TypeAlias format ${data.toList}")
          }
          TypeAlias(abbreviation, originName, originFullName)
      }
  }

  override def store(value: Constant)(implicit isabelle: Isabelle): MLValue[Constant] = {
    val data = value match {
      case ConstDef(printName, fullName, normDef, deps) =>
        DList(
          DString("ConstDef"),
          DString(printName),
          DString(fullName),
          DString(normDef),
          DList(deps.map(DString): _*)
        )
      case TypeDef(printName, fullName, accessors) =>
        DList(
          DString("TypeDef"),
          DString(printName),
          DString(fullName),
          DList(accessors.map { case Accessor(a, b, c) =>
            DList(DString(a), DString(b), DString(c))
          }: _*)
        )
      case ConstAlias(abbreviation, originName, originFullName) =>
        DList(DString("ConstAlias"), DString(abbreviation), DString(originName), DString(originFullName))
      case Overloading(overloaded, instance) =>
        DList(DString("Overloading"), DString(overloaded), DString(instance))
      case TypeAlias(abbreviation, originName, originFullName) =>
        DList(DString("TypeAlias"), DString(abbreviation), DString(originName), DString(originFullName))
    }
    Ops.storeConstant(data)
  }
}

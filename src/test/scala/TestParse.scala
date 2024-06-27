import org.scalatest.funsuite.AnyFunSuite
import xk.luan.IsaUtils

class TestParse extends AnyFunSuite {
  val isaHome = "/home/xiaokun/opt/Isabelle2023"

  test("test parsing HOL.Nat") {
    val session = "HOL"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Nat.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing Main") {
    val session = "HOL"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Main.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing Pure") {
    val session = "Pure"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/Pure/Pure.thy"
      IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing HOL-Analysis.Convex") {
    val session = "HOL-Analysis"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Analysis/Convex.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing HOLCF-ex.Dnat") {
    val session = "HOLCF-ex"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/HOLCF/ex/Dnat.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing Completeness.Completeness") {
    val session = "Completeness"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Completeness/Completeness.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }

  test("test parsing Completeness.PermutationLemmas") {
    val session = "Completeness"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Completeness/PermutationLemmas.thy"
    IsaUtils.main(Array("parse", session, isaHome, thyPath, "--ignore"))
  }
}
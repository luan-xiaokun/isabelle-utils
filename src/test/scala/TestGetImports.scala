import org.scalatest.funsuite.AnyFunSuite
import xk.luan.IsaUtils

class TestGetImports extends AnyFunSuite {
  val isaHome = "/home/xiaokun/opt/Isabelle2023"


  test("test parsing HOL.Nat") {
    val session = "HOL"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Nat.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Main") {
    val session = "HOL"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Main.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Pure") {
    val session = "Pure"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/Pure/Pure.thy"
    // should print nothing
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing HOL-Analysis.Convex") {
    val session = "HOL-Analysis"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Analysis/Convex.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing HOLCF-ex.Dnat") {
    val session = "HOLCF-ex"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/HOLCF/ex/Dnat.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing LCF.Ex3") {
    val session = "LCF"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/LCF/ex/Ex3.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Completeness.Completeness") {
    val session = "Completeness"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Completeness/Completeness.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Completeness.PermutationLemmas") {
    val session = "Completeness"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Completeness/PermutationLemmas.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing LCF.Ex1") {
    val session = "LCF"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/LCF/ex/Ex1.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing HOL-UNITY.Counter") {
    val session = "HOL-UNITY"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/UNITY/Comp/Counter.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Tutorial.Nested1") {
    val session = "Tutorial"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/Doc/Tutorial/Recdef/Nested1.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Binding_Syntax_Theory.Univ") {
    val session = "Tutorial"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Binding_Syntax_Theory/Univ.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Iptables_Semantics_Examples_Big.Univ") {
    val session = "Tutorial"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Binding_Syntax_Theory/Univ.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test HOL-Predicate_Compile_Examples.Hotel_Example_Small_Generator") {
    val session = "HOL-Predicate_Compile_Examples"
    val thyPath = "/home/xiaokun/opt/Isabelle2023/src/HOL/Predicate_Compile_Examples/Hotel_Example_Small_Generator.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  // afp-2021/thys/Locally-Nameless-Sigma/Sigma/ParRed.thy
  test("test parsing Locally-Nameless-Sigma.ParRed") {
    val session = "Locally-Nameless-Sigma"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Locally-Nameless-Sigma/Sigma/ParRed.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Regression_Test_Selection.RTS") {
    val session = "Regression_Test_Selection"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Regression_Test_Selection/RTS.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }

  test("test parsing Simple_Firewall.Generic_SimpleFw") {
    val session = "Simple_Firewall"
    val thyPath = "/home/xiaokun/afp-repo/afp-2023/thys/Simple_Firewall/Generic_SimpleFw.thy"
    IsaUtils.main(Array("get_imports", session, isaHome, thyPath))
  }
}

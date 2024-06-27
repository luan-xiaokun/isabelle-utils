import org.scalatest.funsuite.AnyFunSuite
import xk.luan.{ConstantConverter, IsaUtils, IsabelleProcess}

class TestConstant extends AnyFunSuite {
  implicit val constantConverter: ConstantConverter.type = ConstantConverter
  val isaHome = "/home/xiaokun/opt/Isabelle2023"

  test("test constant1") {
    val isabelleProcess =
      new IsabelleProcess(os.Path(isaHome), "HOL-Examples", os.Path("/home/xiaokun/opt/Isabelle2023/src/HOL"))
    isabelleProcess.extractConstants(os.Path("/home/xiaokun/opt/Isabelle2023/src/HOL/Examples/Functions.thy"))
  }

  test("test constant2") {
    val isabelleProcess =
      new IsabelleProcess(os.Path(isaHome), "HOL", os.Path("/home/xiaokun/opt/Isabelle2023/src/HOL"))
    isabelleProcess.extractConstants(os.Path("/home/xiaokun/opt/Isabelle2023/src/HOL/Num.thy"))
  }

  test("test isa-utils extract_constants") {
    val args = Array("extract_constants", "HOL", isaHome, "/home/xiaokun/opt/Isabelle2023/src/HOL/Main.thy")
    IsaUtils.main(args)
  }
}

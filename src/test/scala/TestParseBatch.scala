import org.scalatest.funsuite.AnyFunSuite
import xk.luan.IsaUtils

class TestParseBatch extends AnyFunSuite {
  val isaHome = "/home/xiaokun/opt/Isabelle2023"

  test("test parse batch") {
    val session = "Pure-ex"
    IsaUtils.main(Array(
      "parse_batch",
      session,
      isaHome,
      "/home/xiaokun/opt/Isabelle2023/src/Pure/Pure.thy",
      "/home/xiaokun/opt/Isabelle2023/src/Pure/ML_Bootstrap.thy",
      "/home/xiaokun/opt/Isabelle2023/src/Pure/ex/Guess.thy",
      "--ignore"
    ))
  }
}
package xk.luan

import upickle.default.{ReadWriter => RW, macroRW}

case class Command (name: String, code: String, pos: (Int, Int, Int)) {
  override def toString: String = s"Command(name=$name, code=$code, pos=$pos)"
}

object Command {
  implicit val rw: RW[Command] = macroRW
  def apply(cmd: (String, String, (Int, Int, Int))): Command = Command(cmd._1, cmd._2, cmd._3)
}
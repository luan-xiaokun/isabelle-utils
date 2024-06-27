package xk.luan

import upickle.default.{ReadWriter => RW, macroRW}
import de.unruh.isabelle.control.{Isabelle, IsabelleMLException}
import de.unruh.isabelle.mlvalue.Implicits._
import de.unruh.isabelle.mlvalue.{MLFunction2, MLValue}
import de.unruh.isabelle.pure.Implicits._
import de.unruh.isabelle.pure.{Theory, ToplevelState, Transition}
import xk.luan.Constant.constantConverter

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

class ConstantExtractor()(implicit isabelle: Isabelle) {
  private val injectionCode = Source.fromResource("constant_extractor.ml").getLines().mkString("\n")

  def extract(thy: Theory, initState: ToplevelState, code: String): List[ConstantExtractionEntry] = {
    val extractedConstantsMap = collection.mutable.ListBuffer[ConstantExtractionEntry]()
    val transitionTextPairs = Transition.parseOuterSyntax(thy, code)
    val injection = Transition.parseOuterSyntax(thy, s"ML \\<open>${injectionCode}\\<close>")

    var cmdToExtract = ("", "", (0, 0, 0))
    var safeParse: MLFunction2[ToplevelState, String, List[Constant]] = null

    def isProbeState(st: ToplevelState): Boolean =
      safeParse != null && !st.isEndTheory && !st.isProofMode

    var state = initState
    transitionTextPairs.foreach { case (transition, text) =>
      val inProof = state.isProofMode
      state = transition.execute(state)
      if (!inProof) {
        val pos = transition.position
        // replace all regex "\s+" with " "
        val c = text.replaceAll("\\s+", " ")
        cmdToExtract = (transition.name, c, (pos.line.get, pos.offset.get, pos.endOffset.get))
      }

      if (text.startsWith("theory ") && safeParse == null) {
        injection.foreach { case (inj, _) => state = inj.execute(state) }
        // also import the ML structure into the global ML environment, but with a different name
        Theory.Ops
          .importMLStructure(state.theory, "ConstantExtractor", "ConstantExtractorIsaUtils")
          .force
          .retrieve: Unit
        // IMPORTANT make sure that we register E_Constant properly
        Constant.init()
        safeParse =
          MLValue.compileFunction[ToplevelState, String, List[Constant]]("ConstantExtractorIsaUtils.safe_parse")
      }

      // maybe we could parse less frequently, only deal with those specific commands
      if (text.trim.nonEmpty && isProbeState(state)) {
        val constants =
          try {
            Await.result(safeParse(state, cmdToExtract._2).force.retrieve, Duration.Inf)
          } catch {
            case e: IsabelleMLException => println(e); List.empty
          }
        if (constants.nonEmpty)
          extractedConstantsMap += ConstantExtractionEntry(Command(cmdToExtract), constants)
      }
    }
    extractedConstantsMap.toList
  }
}

case class ConstantExtractionEntry(command: Command, constants: List[Constant])

object ConstantExtractionEntry {
  implicit val rw: RW[ConstantExtractionEntry] = macroRW
}
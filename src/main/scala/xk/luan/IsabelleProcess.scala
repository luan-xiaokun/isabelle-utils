package xk.luan

import de.unruh.isabelle.control.{Isabelle, IsabelleMLException}
import de.unruh.isabelle.pure.{Theory, TheoryHeader, ToplevelState}
import upickle.default._

class IsabelleProcess(
  val isaHome: os.Path,
  val session: String,
  val workingDirectory: os.Path,
  val afpHome: Option[os.Path] = None
) {
  // TODO: maybe set the workingDirectory properly
  // TODO: we haven't tested on '../a/b' imports and some other corner cases
  private val setup = Isabelle.Setup(
    isabelleHome = isaHome.toNIO,
    logic = session,
    userDir = None,
    workingDirectory = workingDirectory.toNIO,
    sessionRoots = if (afpHome.isEmpty) Nil else Seq(afpHome.get.toNIO)
  )
  implicit val isabelle: Isabelle = new Isabelle(setup)
  private val theoryManager = new TheoryManager(session)
  private val constantExtractor = new ConstantExtractor()

  private val sessionFilesMap: Map[String, List[os.Path]] = {
    if (os.exists(workingDirectory / "ROOT"))
      Utils.parseRootFile(workingDirectory / "ROOT")
    else {
      Map(
        session ->
          os.list(workingDirectory).filter(_.toString.endsWith(".thy")).toList
      )
    }
  }

  def parseBatch(thyPaths: List[os.Path], ignore: Boolean = false): List[String] = {
    thyPaths.map(parse(_, ignore))
  }

  def parse(thyPath: os.Path, ignore: Boolean = false): String = {
    val thyName = thyPath.last.dropRight(4)
    val fullName = s"$session.$thyName"
    val rawResults = {
      try {
        theoryManager.parseTheory(Theory(fullName), os.read(thyPath), ignore)
      } catch {
        case e: IsabelleMLException =>
          List(("ERROR", e.getMessage, (-1, -1, -1)))
      }
    }
    rawResults
      .map { case (name, code, pos) =>
        (Utils.quote(name), Utils.quote(code), pos.toString).productIterator.mkString("\n")
      }
      .mkString("\n")
  }

  def getImports(thyPath: os.Path): String = {
    val imports = TheoryHeader.read(os.read(thyPath)).imports
    val fullNameImports =
      imports.map(name =>
        if (isGlobalThyName(name) || name.startsWith("~~/src/")) name
        else if (!name.contains(".") && !name.contains("/")) s"$session.$name"
        else name
      )
    val importedFiles = fullNameImports.map(file =>
      if (file.startsWith("~~/src/"))
        Some(os.RelPath(file.drop(3) + ".thy").resolveFrom(isaHome).toNIO)
      else if (file.startsWith("..") || file.contains("/"))
        Some(os.RelPath(file + ".thy").resolveFrom(thyPath / os.up).toNIO)
      else
        theoryManager.findTheoryFile(file)
    )
    importedFiles
      .map(path => path.map(_.toString).getOrElse("NOT FOUND"))
      .mkString("\n")
  }

  private def isGlobalThyName(thyName: String): Boolean = {
    val globalThys =
      List("IFOLP", "FOLP", "IFOL", "FOL", "Main", "Complex_Main", "HOLCF", "Pure", "ML_Bootstrap", "ZF", "ZFC")
    globalThys.contains(thyName)
  }

  def extractConstants(thyPath: os.Path, outputPath: Option[os.Path] = None): String = {
    val (thy, state) = initialize(thyPath)
    val commandConstantsMap = constantExtractor.extract(thy, state, os.read(thyPath))
    val result = write(commandConstantsMap)
    if (outputPath.nonEmpty)
      os.write.over(outputPath.get, result)
    result
//    val extractedConstants = collection.mutable.ListBuffer[Constant]()
//    val transitionTextPairs = Transition.parseOuterSyntax(thy, os.read(thyPath))
//    // read from resources/constant_extractor.ml
//    val injectionCode = Source.fromResource("constant_extractor.ml").getLines().mkString("\n")
//    val injection = Transition.parseOuterSyntax(thy, s"ML \\<open>${injectionCode}\\<close>")
//
//    var codeToExtract = ""
//    var safeParse: MLFunction2[ToplevelState, String, List[Constant]] = null
//
//    // decide if we are going to do the extraction in this state
//    def isProbeState(state: ToplevelState): Boolean =
//      safeParse != null && !state.isEndTheory && !state.isProofMode
//
//    transitionTextPairs.foreach { case (transition, text) =>
//      if (!state.isProofMode) codeToExtract = text
//      state = transition.execute(state)
//
//      if (text.startsWith("theory ") && safeParse == null) {
//        // execute ML code in constant_extractor.ml
//        injection.foreach { case (inj, _) => state = inj.execute(state) }
//        // also import the ML structure into the global ML environment, but with a different name
//        Theory.Ops.importMLStructure(state.theory, "ConstantExtractor", "ConstantExtractorIsaUtils").force.retrieve: Unit
//        // IMPORTANT make sure that we register E_Constant properly
//        Constant.init()
//        safeParse =
//          MLValue.compileFunction[ToplevelState, String, List[Constant]]("ConstantExtractorIsaUtils.safe_parse")
//      }
//
//      // maybe we could parse less frequently, only deal with those specific commands
//      if (text.trim.nonEmpty && isProbeState(state)) {
//        val constants =
//          try {
//            Await.result(safeParse(state, codeToExtract).force.retrieve, Duration.Inf)
//          } catch {
//            case e: IsabelleMLException => println(e.toString); List()
//          }
//        extractedConstants ++= constants
//      }
//    }
//    extractedConstants.mkString("\n")
  }

  private def initialize(thyPath: os.Path): (Theory, ToplevelState) = {
    val thyText = os.read(thyPath)
    val thy = theoryManager.beginTheory(thyText, thyPath, sessionFilesMap)
    val state = theoryManager.initToplevel()
    (thy, state)
  }
}

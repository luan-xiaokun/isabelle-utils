package xk.luan

import scopt.OParser

import java.io.File

object IsaUtils {
  def main(args: Array[String]): Unit = {

    // 1st argument: task, including `parse` and `get_imports`
    // 2nd argument: session
    // 3rd argument: filepath
    // 4nd argument: isabelle folder
    // flag: --ignore, ignore all blank lines and comments
    // flag: --afp, path of AFP theory folder

    case class Config(
      task: String,
      session: String,
      isaHome: os.Path,
      filepaths: Seq[os.Path],
      ignore: Boolean,
      afpHome: Option[os.Path],
      output: Option[File] = None
    )

    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        programName("isa-utils"),
        head("prise", "0.1"),
        help("help").text("prints this usage text"),
        arg[String]("task")
          .action((x, c) => c.copy(task = x))
          .text("task to be executed"),
        arg[String]("session")
          .action((x, c) => c.copy(session = x))
          .text("session which the theory file belongs to"),
        arg[File]("isa_home")
          .action((x, c) => c.copy(isaHome = os.Path(x.toPath)))
          .text("the isabelle distribution folder"),
        arg[File]("filepath...")
          .unbounded()
          .action((x, c) => c.copy(filepaths = c.filepaths :+ os.Path(x.toPath)))
          .text("the theory file to be processed"),
        opt[Unit]("ignore")
          .action((_, c) => c.copy(ignore = true))
          .text("ignore all blank lines and comments"),
        opt[File]("afp")
          .action((x, c) => c.copy(afpHome = Some(os.Path(x.toPath))))
          .text("path of AFP theory folder"),
        opt[File]("output")
          .action((x, c) => c.copy(output = Some(x)))
          .text("output file path")
      )
    }

    OParser.parse(parser, args, Config("", "HOL", os.pwd, Seq(), ignore = false, None)) match {
      case Some(config) =>
        assert(config.filepaths.nonEmpty)
        // search for working directory from config.filepaths.head / os.up, until we find a ROOT file
        val workingDirectory = Utils.findNearestRootFolder(config.filepaths.head).get
        val isaProcess =
          new IsabelleProcess(config.isaHome, config.session, workingDirectory, config.afpHome)
        config.task match {
          case "parse" =>
            assert(config.filepaths.length == 1)
            println(isaProcess.parse(config.filepaths.head, config.ignore))
          case "parse_batch" =>
            val parseResults = isaProcess.parseBatch(config.filepaths.toList, config.ignore)
            config.filepaths.zip(parseResults).foreach { case (path, result) =>
              println(f"Theory path: $path\n$result")
            }
          case "get_imports" =>
            assert(config.filepaths.length == 1)
            println(isaProcess.getImports(config.filepaths.head))
          case "get_imports_batch" =>
            assert(config.filepaths.nonEmpty)
            config.filepaths.foreach { path =>
              val result = isaProcess.getImports(path)
              println(f"Theory path: $path\n$result")
            }
          case "extract_constants" =>
            assert(config.filepaths.length == 1)
            println(isaProcess.extractConstants(config.filepaths.head))
          case _ => println(f"Unrecognized task '${config.task}'")
        }
      case _ =>
    }
  }
}

package xk.luan

import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  def parseRootFile(rootPath: os.Path): Map[String, List[os.Path]] = {
    val projectPath = rootPath / os.up
    // Step 1. remove comment blocks
    val rootText = os.read(rootPath).replaceAll(raw"\(\*[\s\S]*?\*\)", "")

    // Step 2. find all 'session' definitions in rootText
    val directorySessionMap = collection.mutable.Map[Path, String]()
    val sessionNameList = ListBuffer[String]()
    val mainDirectoryList = ListBuffer[Path]()
    val sessionNameIndexList = ListBuffer[Int]()
    val sessionRegex =
      """session\s+([\w_"+-]+)\s*(?:\(.*?\)\s*)?(?:in\s*(.*?)\s*)?=.*?""".r
    sessionRegex
      .findAllIn(rootText)
      .matchData
      .foreach(m => {
        val name = m.group(1).stripPrefix("\"").stripSuffix("\"")
        val mainDirectory = m.group(2) match {
          // getting null means this session is the AFP main session
          case null => ""
          case dir  => dir.stripPrefix("\"").stripSuffix("\"")
        }
        sessionNameList += name
        mainDirectoryList += projectPath.toNIO.resolve(mainDirectory)
        sessionNameIndexList += m.start(1)
      })
    sessionNameIndexList += rootText.length

    // Step 3. find all directories between session definitions
    val directoriesPattern = """directories\s*\n([\s\S]*?)theories""".r
    val sessionRangeList = sessionNameIndexList.sliding(2)
    sessionRangeList zip sessionNameList zip mainDirectoryList foreach { case ((indices, sessionName), mainDirectory) =>
      directoriesPattern
        .findFirstMatchIn(rootText.slice(indices.head, indices.last)) match {
        case Some(raw_match) =>
          val directories = raw_match
            .group(1)
            .trim
            .split("\\s+")
            .map(dir => dir.stripSuffix("\"").stripPrefix("\""))
          directorySessionMap += (mainDirectory -> sessionName)
          // note that these directories are relative to mainDirectory!
          directorySessionMap ++= directories.map(dir => mainDirectory.resolve(dir).normalize() -> sessionName)
        case None => directorySessionMap += (mainDirectory -> sessionName)
      }
    }

    // Step 4. assign each thy file in the AFP entry directory to its session
    val sessionFilesMap = sessionNameList.map(_ -> ListBuffer[os.Path]()).toMap

    @tailrec
    def getDir(path: os.Path): Option[os.Path] = {
      if (directorySessionMap.contains(path.toNIO)) Some(path)
      else if (path == os.root) None
      else getDir(path / os.up)
    }

    getThyFiles(projectPath).foreach(thyFile => {
      val dir = getDir(thyFile).fold(projectPath)(x => x).toNIO
      val sessionName = directorySessionMap(dir)
      sessionFilesMap(sessionName) += thyFile
    })

    sessionFilesMap.transform((_, v) => v.toList)
  }

  private def getThyFiles(file: os.Path): List[os.Path] = {
    val these = os.list(file).toList
    these.filter(os.isFile).filter(_.last.endsWith(".thy")) ++ these
      .filter(os.isDir)
      .flatMap(p => getThyFiles(p))
  }

  private def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ =>
      if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else String.valueOf(ch)
  }

  def quote(s: String): String = "\"" + escape(s) + "\""

  def escape(s: String): String = s.flatMap(escapedChar)

  def findNearestRootFolder(path: os.Path): Option[os.Path] = {
    @tailrec
    def helper(currentPath: os.Path): Option[os.Path] = {
      if (os.exists(currentPath / "ROOT")) Some(currentPath)
      else if (currentPath == os.root) None
      else helper(currentPath / os.up)
    }
    helper(path)
  }
}

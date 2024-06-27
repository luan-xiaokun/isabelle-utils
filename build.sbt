ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "isa-utils"
  )

libraryDependencies += "de.unruh" %% "scala-isabelle" % "0.4.2"  // release
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.3.1"

ThisBuild / assemblyMergeStrategy := {
  case x if x.contains("de/unruh") => MergeStrategy.first
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case x if x.contains("META-INF") => MergeStrategy.first
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

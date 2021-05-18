import Dependencies._

ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "kr.ac.kaist.pyanalyzer"

mainClass in (Compile, run) := Some("kr.ac.kaist.pyanalyzer.PyAnalyzer")

lazy val root = (project in file("."))
  .settings(
    name := "python-analyzer",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
    )  
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

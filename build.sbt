import Dependencies._

ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "kr.ac.kaist.pyanalyzer"

mainClass in (Compile, run) := Some("kr.ac.kaist.pyanalyzer.PyAnalyzer")

lazy val prodTest = taskKey[Unit]("Launch production test")
lazy val fileParseTest = taskKey[Unit]("Launch file test")

lazy val root = (project in file("."))
  .settings(
    name := "python-analyzer",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.9" % "test",
      "org.jline" % "jline" % "3.13.3",
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDG"),
    parallelExecution in Test := true,
    prodTest := (testOnly in Test).toTask(" *ProdTest").value,
    fileParseTest := (testOnly in Test).toTask(" *FileParseTest").value,
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

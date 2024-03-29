import Dependencies._

ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "kr.ac.kaist.pyanalyzer"

mainClass in (Compile, run) := Some("kr.ac.kaist.pyanalyzer.PyAnalyzer")

lazy val apiAnalysisTest = taskKey[Unit]("Launch API analysis test")
lazy val classAnalysisTest = taskKey[Unit]("Launch class analysis test")
lazy val classOrderTest = taskKey[Unit]("Launch class order test")
lazy val fileParseTest = taskKey[Unit]("Launch file test")
lazy val fileTransformTest = taskKey[Unit]("Launch file transform test")
lazy val prodTest = taskKey[Unit]("Launch production test")

lazy val root = (project in file("."))
  .settings(
    name := "python-analyzer",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % "1.8",
      "org.jline" % "jline" % "3.20.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.9" % "test",
      "io.spray" %%  "spray-json" % "1.3.6",
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
    parallelExecution in Test := true,
    apiAnalysisTest := (testOnly in Test).toTask(" *APIAnalysisTest").value,
    classAnalysisTest := (testOnly in Test).toTask(" *ClassAnalysisTest").value,
    classOrderTest := (testOnly in Test).toTask(" *ClassOrderTest").value,
    fileParseTest := (testOnly in Test).toTask(" *FileParseTest").value,
    fileTransformTest := (testOnly in Test).toTask(" *FileTransformTest").value,
    prodTest := (testOnly in Test).toTask(" *ProdTest").value,
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner.runPipe
import java.io.File
import scala.Console._

object GenTest {
  def genTest(testName: String, path: String): Unit = {
    try runPipe(path, s"$TEST_DIR/regress/$testName")
    catch {
      case e: Throwable =>
        println(s"$YELLOW[Warning] $testName failed!$RESET")
        e.printStackTrace
    }
  }

  def apply(optionMap: Map[String, String]): Unit = {
    // hvd test
    val hvdDir = new File(HOROVOD_DIR) // TODO: add test cases
    for {
      version <- hvdDir.listFiles if version.isDirectory
      api <- version.listFiles if api.isDirectory
      model <- api.listFiles if model.isDirectory
    } {
      val versionName = version.getName
      val apiName = api.getName
      val modelName = model.getName
      val targetDirName =
        if (model.listFiles.exists(_.getName == "org_mod")) "org_mod" else "org"
      val testName = s"$versionName-$apiName-$modelName"
      genTest(testName, s"$model/$targetDirName")
    }

    // tutorial test
    val tutoDir = new File(TUTORIAL_DIR) // TODO: add test cases
    val tutoModelPattern = "\\d\\d.*".r
    val multipleModels = List(
      "01-TF2.0-Overview",
      "05-FashionMNIST",
    )
    def containsMultipleModel(modelDir: String): Boolean =
      multipleModels contains modelDir



    tutoDir.listFiles.foreach (candidate => {
      candidate.getName match {
        case modelDir if containsMultipleModel(modelDir) =>
          executeCmd(s"""bash -c "rm $TEST_DIR/regress/$modelDir/*"""")
          candidate.listFiles.foreach(file =>
            if (file.toString endsWith ".py")
              genTest(modelDir, file.toString)
          )
        case modelDir if tutoModelPattern matches modelDir =>
          executeCmd(s"""bash -c "rm $TEST_DIR/regress/$modelDir/*"""")
          genTest(modelDir, candidate.toString)
        case _ =>
      }
    })
  }
}

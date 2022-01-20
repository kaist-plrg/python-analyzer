package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner.runPipe
import java.io.File
import scala.Console._

object Regress {
  def apply(optionMap: Map[String, String]): Unit = {
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
      executeCmd(s"""bash -c "rm $TEST_DIR/regress/$testName/*"""")
      try runPipe(s"$model/$targetDirName", s"$TEST_DIR/regress/$testName")
      catch {
        case e: Throwable =>
          println(s"$YELLOW[Warning] $testName failed!$RESET")
          e.printStackTrace
      }
    }
  }
}

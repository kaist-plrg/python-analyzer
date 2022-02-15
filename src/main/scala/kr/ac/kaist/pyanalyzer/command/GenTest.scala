package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner.runPipe
import java.io.File
import scala.Console._

object GenTest {
  def apply(optionMap: Map[String, String]): Unit = {
    iterTests(genTest)
  }

  def genTest(dirname: String, path: String): Unit = {
    try runPipe(path, s"$TEST_DIR/regress/$dirname")
    catch {
      case e: Throwable =>
        println(s"$YELLOW[Warning] $dirname failed!$RESET")
        e.printStackTrace
    }
  }

  def iterTests(f: (String, String) => Unit): Unit = {
    // horovod example
    val hvdDir = new File(HOROVOD_DIR)
    for {
      version <- hvdDir.listFiles if version.isDirectory
      api <- version.listFiles if api.isDirectory
      model <- api.listFiles if model.isDirectory
    } {
      val targetDirName =
        if (model.listFiles.exists(_.getName == "org_mod")) "org_mod" else "org"
      val testName =
        s"HVD$model".diff(s"$hvdDir").replace('/', '-')
      f(testName, s"$model/$targetDirName")
    }

    // tutorial example 1
    val tutoDir = new File(TUTORIAL_DIR)
    tutoDir.listFiles.foreach { exDir =>
      val exDirName = exDir.getName

      // tutorial example directory that contains multiple mainscipts
      if (TUTO_MAINSCRIPTS_MAP contains exDirName)
        tutoMainscriptOf(exDirName).foreach{ ms =>
          val additionalName = ms.dropRight(3).replace("_", "-")
          f(s"TUTO1-$exDirName-$additionalName", s"$exDir/$ms")
        }

      // normal tutorial example directory
      else if (tutoModelPattern matches exDirName)
        f(s"TUTO1-$exDirName", s"$exDir")
    }

    // tutorial example 2
    val tutoDir2 = new File(TUTORIAL_DIR_2)
    tutoDir2.listFiles.foreach { file =>
      val testName = s"TUTO2-${file.getName.replace("_", "-").dropRight(3)}"
      f(testName, file.toString)
    }

    // CNN text classification example
    f("CNN-text-classification", CNN_DIR)
  }

  val tutoModelPattern = "\\d\\d.*".r
  // tutorial test
  val TUTO_MAINSCRIPTS_MAP = Map(
    "01-TF2.0-Overview" -> List(
      "conv_train.py",
      "fc_train.py"
    ),
    "05-FashionMNIST" -> List(
      "mnist_Seqential_gradient.py",
      "mnist_fit.py",
      "mnist_custommodel.py",
      "mnist_matmul.py",
    ),
    "21-CN-EN-Translation-BERT" -> List(
      "bert_train.py",
      "transformer_train.py",
    ),
  )
  def tutoMainscriptOf(model: String): List[String] =
    TUTO_MAINSCRIPTS_MAP(model)
}

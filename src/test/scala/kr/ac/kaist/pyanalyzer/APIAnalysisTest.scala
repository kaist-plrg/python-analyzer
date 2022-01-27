package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.transformer._
import org.scalatest.funsuite._
import java.io.File
import scala.Console._

class APIAnalysisTest extends AnyFunSuite {
  val help = s"""Test API analysis"""

  // check model
  val modelPattern = "\\d\\d.*".r
  val multipleModels = List(
    "01-TF2.0-Overview",
    "05-FashionMNIST",
  )
  def containsMultipleModel(modelDir: String): Boolean =
    multipleModels contains modelDir

  // test model
  def testAPIPattern(name: String, path: String): Unit =
    test(name) {
      try {
        val targetOpt = CheckFilePipe!!(path)
        val fs = PathPipe!!(path)
        val orgASTs = ParsePipe!!(fs)
        val classOrder = ClassPipe!!(orgASTs)
        val loopTypes = InfoTLPipe!!((orgASTs, classOrder))
        val (_, api) =
          MainScriptPipe!!((orgASTs, loopTypes, targetOpt))
        if (api == Bot) throw APIException
      } catch {
        case e: RuntimeException => cancel
        case APIException => fail
      }
    }

  // test models
  val tutoDir = new File(TUTORIAL_DIR) // TODO: add test cases
  tutoDir.listFiles.foreach (candidate => {
    candidate.getName match {
      case modelDir if containsMultipleModel(modelDir) =>
        candidate.listFiles.foreach(file =>
          if (file.toString endsWith ".py")
            testAPIPattern(s"$modelDir/${file.getName}", file.toString)
        )
      case modelDir if modelPattern matches modelDir =>
        testAPIPattern(modelDir, candidate.toString)
      case _ =>
    }
  })
}

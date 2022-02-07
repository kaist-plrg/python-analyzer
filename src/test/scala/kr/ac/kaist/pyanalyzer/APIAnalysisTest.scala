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
  tutoDir.listFiles.foreach (
    candidate => candidate.getName match {
      case modelDir if tutoModelPattern matches modelDir =>
        mainscriptOf(modelDir).foreach(
          ms => testAPIPattern(s"$modelDir/$ms", s"$candidate/$ms")
        )
      case _ =>
    }
  )
}

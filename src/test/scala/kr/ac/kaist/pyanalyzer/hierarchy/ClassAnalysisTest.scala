package kr.ac.kaist.pyanalyzer.hierarchy

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassAnalysis._
import kr.ac.kaist.pyanalyzer.util.Useful._
import java.io.File

class ClassAnalysisTest extends AnyFunSuite {
  var help = """Tests CHA."""

  val logPath = s"$TEST_LOG_DIR/ClassAnalysisTest"
  implicit val logWriter = getPrintWriter(logPath)
  implicit val setPrompt = true

  def testDir(dir: String) = test(s"ClassAnalysisTest:$dir"){
    val file = new File(dir)   
    val result: ClassOrder = dirClassOrder(file) 
    prompt(result.toString())
  }

  val TF2DIR = s"$HOROVOD_DIR/Tensorflow2" 
  val TF1DIR = s"$HOROVOD_DIR/Tensorflow1"

  def init: Unit = {
    prompt(help)
    testDir(s"$TF2DIR/DistributedGradientTape/ResNet-ImageNet1K/org")
    testDir(s"$TF2DIR/DistributedGradientTape/VGG-CIFAR10/org")
    testDir(s"$TF2DIR/DistributedGradientTape/Yolo_V3/org")
  }


  init

}

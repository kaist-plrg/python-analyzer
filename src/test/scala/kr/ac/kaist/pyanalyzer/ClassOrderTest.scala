package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Useful._

class ClassOrderTest extends AnyFunSuite {
  var help = """Tests transfer funcitons of ClassOrder."""

  val logPath = s"$TEST_LOG_DIR/ClassOrderTest"
  implicit val logWriter = getPrintWriter(logPath)
  implicit val setPrompt = true

  def testImports(): Unit = test("ClassOrderTest:Imports"){
    val initOrder = ClassOrder(Map(), Map()) 
    prompt(s"initOrder: $initOrder")

    val tfAlias = Alias(List(Id("tensorflow")), Some(Id("tf")))
    val tfImport = ImportStmt(List(tfAlias))
    val ord = transferStmt(initOrder)(tfImport)
    prompt(s"after tf import: $ord")

    val kerasAlias = Alias(List(Id("keras")), None)
    val kerasImport = ImportFromStmt(0, List(Id("tensorflow")), List(kerasAlias))
    val ord2 = transferStmt(ord)(kerasImport)
    prompt(s"after keras import: $ord2")

    val modelExpr = Attribute(EName(Id("keras")), Id("Model"))
    val resnetClass = ClassDef(Nil, Id("Resnet"), List(modelExpr), Nil, Nil) 
    val ord3 = transferStmt(ord2)(resnetClass)
    prompt(s"after resnet class: $ord3")

    val resnetExpr = EName(Id("Resnet"))
    val resnet2Class = ClassDef(Nil, Id("Resnet2"), List(resnetExpr), Nil, Nil)
    val ord4 = transferStmt(ord3)(resnet2Class)
    prompt(s"after resnet2 class: $ord4")

    val resnet2Name = Fullname(List("Resnet2"))
    val modelName = Fullname(List("tensorflow", "keras", "Model"))
    assert(ord4.isSubclass(resnet2Name, modelName))
  }
 
  def init: Unit = {
    prompt(help)
    testImports()
  }

  init
}

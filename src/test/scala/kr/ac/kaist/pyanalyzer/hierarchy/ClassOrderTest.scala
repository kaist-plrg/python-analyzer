package kr.ac.kaist.pyanalyzer.hierarchy

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import org.scalatest.Assertion
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.Transformer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Useful._

class ClassOrderTest extends AnyFunSuite {
  var help = """Tests transfer functions of ClassOrder."""

  val logPath = s"$TEST_LOG_DIR/ClassOrderTest"
  implicit val logWriter = getPrintWriter(logPath)
  implicit val setPrompt = true

  def promptLine() = prompt("====================")
  def promptline() = prompt("--------------------")

  def testImports(): Unit = test("ClassOrderTest:Imports"){
    val initOrder = ClassOrder() 
    prompt(s"initOrder\n$initOrder")

    val tfAlias = Alias(List(Id("tensorflow")), Some(Id("tf")))
    val tfImport = parseStmts("import tensorflow as tf")(0)
    val ord = transferStmt(initOrder)(tfImport)
    prompt(s"after tf import\n$ord")
    promptLine()

    val kerasAlias = Alias(List(Id("keras")), None)
    val kerasImport = ImportFromStmt(0, List(Id("tensorflow")), List(kerasAlias))
    val ord2 = transferStmt(ord)(kerasImport)
    prompt(s"after keras import\n$ord2")
    promptLine()

    val modelExpr = Attribute(EName(Id("keras")), Id("Model"))
    val resnetClass = ClassDef(Nil, Id("Resnet"), List(modelExpr), Nil, Nil) 
    val ord3 = transferStmt(ord2)(resnetClass)
    prompt(s"after resnet class\n$ord3")
    promptLine()

    val resnetExpr = EName(Id("Resnet"))
    val resnet2Class = ClassDef(Nil, Id("Resnet2"), List(resnetExpr), Nil, Nil)
    val ord4 = transferStmt(ord3)(resnet2Class)
    prompt(s"after resnet2 class\n$ord4")
    promptLine()

    val resnet2Name = Fullname(List("Resnet2"))
    val modelName = Fullname(List("tensorflow", "keras", "Model"))
    assert(ord4.isSubclass(resnet2Name, modelName))
    assert(ord4.isSubclass(modelName, modelName))
  }

  def testcode(
    testname: String, 
    code: String, 
    tester: ClassOrder => Unit,
  ) = test(s"ClassOrderTest:$testname") {
    prompt(s"$code")
    promptline()
    val stmts = parseStmts(code)
    val resOrder =
      stmts.foldLeft(ClassOrder())((o: ClassOrder, s: Stmt) => {
        transferStmt(o)(s)
      })
    prompt(s"$resOrder")
    tester(resOrder)
    promptLine()
  }

  ////////////////////////////////////////////////////

  val basicCode = 
    """import X as Y
      |from A.B.C import D
      |class E(D):
      |  pass
      |class F(Y.Z):
      |  pass""".stripMargin

  val basicTest: ClassOrder => Unit = (order: ClassOrder) => {
    val Ename = parseStrFullname("E")
    val ABCname = parseStrFullname("A.B.C.D")
    assert(order.isSubclass(Ename, ABCname))
  }

  val multiCode =
    """import X as Y
      |import X.Z as zet
      |class A(Y.Z): pass
      |class B(A): pass
      |class F(zet): pass
      |class C(A, F): pass
      |class D(B, C): pass
      |class E(D): pass""".stripMargin

  val multiTest: ClassOrder => Unit = (order: ClassOrder) => {
    val Dname = parseStrFullname("D")
    val Bname = parseStrFullname("B")
    val Cname = parseStrFullname("C")
    val XZname = parseStrFullname("X.Z")
    assert(order.isSubclass(Dname, Bname) && order.isSubclass(Dname, Cname))
    assert(order.isSubclass(Dname, XZname))
  }

////////////////////////////////////////////////////

  def init: Unit = {
    prompt(help)
    testImports()
    testcode("basic", basicCode, basicTest)
    testcode("multi", multiCode, multiTest)
  }

  init
}

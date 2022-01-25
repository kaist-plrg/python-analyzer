package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._
import scala.util.Try

case class Warning(message: String, code: Stmt) {
  override def toString: String =
    s"$YELLOW• ${beautify(code)}  - $message\n$RESET"
}

object Transformer extends Transformer {
  // transformed one AST into another AST
  def apply(module: Module, order: ClassOrder, tl: APIType): Module = {
    // transform
    implicit val env = Env(classOrder=order)
    val (newModule, lw) = tl match {
      case Sess => SessRule(module)
      case MonSess => MonSessRule(module)
      // case Est => EstRule(module)
      case DistGradTape => DistGradTapeRule(module)
      case DistOptim => DistOptimRule(module)
      case Bot =>
        val (body, _, lw) = transform(module.body)
        (module.copy(body=body), lw)
    }

    // log
    lazy val promptModuleName =
      prompt(s"<${module.name}>\n")(false, TRANS_PRINT_WRITER)
    lw.map(warning => {
        promptModuleName
        prompt(s"$warning")(false, TRANS_PRINT_WRITER)
    })
    newModule
  }
}

// Transform rule
trait Transformer extends TransformerWalker {
  /////////////////////////////////////////
  // helper functions
  /////////////////////////////////////////
  def parseStmts(code: String): List[Stmt] = {
    TokenListParser(tokenizeText(code)).body
  }

  def parseExpr(str: String): Expr = {
    val stmts = parseStmts(str)
    stmts.headOption match {
      case Some(ExprStmt(e)) if stmts.size == 1 => e
      case _ => ???
    }
  }

  // TODO: need new id gen algorithm
  def newId: Id = Id("id_new")

  def findKwarg(lk: List[Kwarg], str: String): Option[NormalKwarg] =
    (lk.find {
      case NormalKwarg(Id(x), _) if x == str => true
      case _ => false
    }).asInstanceOf[Option[NormalKwarg]]

  def replaceElement[T](lk: List[T], from: T, to: T): List[T] = lk match {
    case Nil => lk
    case h :: t =>
      if (h == from) to :: t
      else h :: replaceElement(t, from, to)
  }

  // default is change head of le
  def changeArg(
    call: Call,
    name: String,
    changeFunc: Expr => Expr,
    position: Int = 0
  ): Call = {
    val Call(f, le, lk) = call
    findKwarg(lk, name) match {
      case Some(kwarg) =>
        val newKwarg = kwarg.copy(expr=changeFunc(kwarg.expr))
        Call(f, le, replaceElement(lk, kwarg, newKwarg))
      case None => Try(le(position)).toOption match {
        case Some(e) =>
          Call(f, replaceElement(le, e, changeFunc(e)), lk)
        case None =>
          println(s"""$YELLOW[Warning] No argument given: $name
                      |${beautify(call)}$RESET""".stripMargin)
          call
      }
    }
  }

  // default is add
  def changeOrAddArg(
    call: Call,
    name: String,
    e: Expr,
    position: Int = -1
  ): Call = {
    val Call(f, le, lk) = call
    findKwarg(lk, name) match {
      case None if le.length < position || position < 0 =>
        Call(f, le, lk :+ NormalKwarg(Id(name), e))
      case _ =>
        changeArg(call, name, _ => e, position)
    }
  }


  def removeElement[T](lk: List[T], from: T): List[T] = lk match {
    case h :: t => if (h == from) removeElement(t, from) else h :: removeElement(t, from)
    case Nil => Nil
  }

  /////////////////////////////////////////
  // Data needed for transformation
  // TODO this is actually static thingy...
  /////////////////////////////////////////
  def getStmts(name: String): List[Stmt] = getStmts(name, Nil)
  def getStmts(name: String, nodes: Node*): List[Stmt] = getStmts(name, nodes.toList)
  def getStmts(name: String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => ???
    }
  private val codeData: Map[String, List[String] => String] = Map()

  // changeFuncs
  val lrScaling: Expr => Expr =
    e => parseExpr(s"${beautify(e)} * hvd.size()")
}

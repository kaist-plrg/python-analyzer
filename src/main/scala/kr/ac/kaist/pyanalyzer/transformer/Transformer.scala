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
      case GradTape => GradTapeRule(module)
      case Keras => KerasRule(module)
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

  // Utility
  def findKwarg(lk: List[Kwarg], str: String): Option[NormalKwarg] =
    (lk.find {
      case NormalKwarg(Id(x), _) if x == str => true
      case _ => false
    }).asInstanceOf[Option[NormalKwarg]]

  def replaceElement[T](list: List[T], from: T, to: T): List[T] = 
    list.map(elem => if (elem == from) to else elem)

  def removeElement[T](list: List[T], from: T): List[T] = 
    list.filter(elem => elem == from)  

  // Utility fn for transformation
  // Change a keyword-or-positional argument expr in func. call 
  def changeArg(
    callExpr: Call,
    targetKwd: String,
    transFn: Expr => Expr,
    pos: Int = 0
  ): Call = {
    val Call(f, exprs, kwds) = callExpr
    findKwarg(kwds, targetKwd) match {
      // exist in kwd
      case Some(kwarg) => {
        val newKwarg = kwarg.copy(expr=transFn(kwarg.expr))
        Call(f, exprs, replaceElement(kwds, kwarg, newKwarg))
      }
      case None => 
        exprs.lift(pos) match {
          // exist in positional
          case Some(arg) =>
            Call(f, replaceElement(exprs, arg, transFn(arg)), kwds)
          // does not exist -> warning
          case None =>
            println(s"""$YELLOW[Warning] No argument given: $targetKwd
                        |${beautify(callExpr)}$RESET""".stripMargin)
            callExpr
        }
    }
  }

  // Chane a keyword-or-positional argument in func.call,
  // add the argument if does not exist
  def changeOrAddArg(
    callExpr: Call,
    targetKwd: String,
    e: Expr,
    pos: Int = -1
  ): Call = {
    val Call(f, exprs, kwds) = callExpr
    findKwarg(kwds, targetKwd) match {
      // x exist in kwds and exprs short 
      case None if exprs.length < pos || pos < 0 =>
        Call(f, exprs, kwds :+ NormalKwarg(Id(targetKwd), e))
      // exist in kwds -> use changArg
      case _ =>
        changeArg(callExpr, targetKwd, _ => e, pos)
    }
  }

  // changeFuncs
  val lrScaling: Expr => Expr =
    e => parseExpr(s"${beautify(e)} * hvd.size()")


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

}

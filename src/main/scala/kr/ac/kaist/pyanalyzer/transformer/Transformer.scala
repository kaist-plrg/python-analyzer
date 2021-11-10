package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

case class Warning(message: String, code: Stmt) {
  override def toString: String =
    s"$YELLOW• ${beautify(code)}  - $message\n$RESET"
}

object Transformer extends Transformer {
  // transformed one AST into another AST
  def apply(module: Module, order: ClassOrder, tl: TLType): Module = {
    // transform
    implicit val env = Env(classOrder=order)
    val (newModule, lw) = tl match {
      case Sess => SessRule(module)
      case MonSess => MonSessRule(module)
      case Est => EstRule(module)
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
        prompt(s"$warning")(true, TRANS_PRINT_WRITER)
    })
    newModule
  }
}

// Transform rule
trait Transformer extends TransformerWalker {
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case AssignStmt(
      List(Subscript(Attribute(EName(idt), Id("environ")),
      EConst(StringLiteral("CUDA_VISIBLE_DEVICES")))), expr, ty) 
      if env.get("os") contains idt => 
        (Nil, env)
    case _ => super.transform(stmt)
  }

  override def transform(alias: Alias)(implicit env: Env): Env = alias match {
    case Alias(List(x), None) if x.name == "os" =>
      env.add("os", x)
    case Alias(List(x), Some(as)) if x.name == "os" =>
      env.add("os", as)
    case _ => super.transform(alias)
  }

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

  def replaceElement[T](lk: List[T], from: T, to: T): List[T] = {
    lk.zipWithIndex.find(e => e._1 == from) match {
      case Some((e, index)) =>
        lk.slice(0, index) ++ (to :: lk.slice(index + 1, lk.length))
      case None => lk
    }
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
}

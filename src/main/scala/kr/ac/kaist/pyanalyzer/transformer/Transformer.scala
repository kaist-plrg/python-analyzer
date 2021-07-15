package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._

trait Transformer {
  // transformed one AST into another AST
  def apply(ast: Node): Node = ???

  def transform(ast: Node): Node = ast match {
    case Module(body, tyIgnore) => Module(transform(body)(Env())._1, tyIgnore)
  }

  def transform(stmts: List[Stmt])(env: Env): (List[Stmt], Env) = {
    ???
  }

  def transform(stmt: Stmt)(env: Env) : (List[Stmt], Env) = stmt match {
    case FunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(FunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    case AsyncFunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(AsyncFunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    // TODO impl others
  }

  def transform(expr: Expr)(env: Env): Expr = expr match {
    case _ => ???
  }

  def transform(comp: Comprehension)(env: Env): Comprehension = comp match {
    case Comprehension(target, in, conds, async) => ??? 
  }

  def transform(handler: ExcHandler)(env: Env): ExcHandler = handler match {
    case ExcHandler(except, asName, body) => ???
  }

  def transform(al: List[Alias])(env: Env): Env = al.foldLeft(env)((e, a) => transform(a)(e))
  def transform(alias: Alias)(env: Env): Env = alias match {
    case _ => ???
  }

  // name changed because of same type after type erasure
  def transformWithlist(wl: List[WithItem])(env: Env): (List[WithItem], Env) = ???
  def transform(wi: WithItem)(env: Env): (WithItem, Env) = ???

  def transform(mc: MatchCase)(env: Env): MatchCase = ???

  def transform(p: Pattern)(env: Env): Pattern = ???
}

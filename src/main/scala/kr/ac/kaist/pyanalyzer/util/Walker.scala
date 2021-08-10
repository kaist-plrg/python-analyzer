package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.parser.ast._

object Walker extends Walker
class Walker {
  val hello = ""
  // recursively walk over Stmt, apply f to all sub-stmts, and aggregate result by given aggregation
  def walkStmt[T](f: Stmt => T, agg: (T, T) => T, default: T)(stmt: Stmt): T = { 
    val curRes = f(stmt)
    val subRes = stmt match {
      case FunDef(decos, name, args, retType, tyExpr, body) =>
        body.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
      case AsyncFunDef(decos, name, args, retType, tyExpr, body) =>
        body.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
      case ClassDef(decos, name, exprs, kwds, body) =>
        body.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
      case ForStmt(ty, forExopr, inExpr, doStmt, elseStmt) =>
        val doRes: T = doStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val elseRes: T = elseStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        agg(doRes, elseRes)
      case AsyncForStmt(ty, forExopr, inExpr, doStmt, elseStmt) =>
        val doRes: T = doStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val elseRes: T = elseStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        agg(doRes, elseRes)
      case WhileStmt(wExpr, doStmt, elseStmt) =>
        val doRes: T = doStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val elseRes: T = elseStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        agg(doRes, elseRes)
      case IfStmt(cond, thenStmt, elseStmt) =>
        val thenRes: T = thenStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val elseRes: T = elseStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        agg(thenRes, elseRes)
      case WithStmt(ty, is, doStmt) =>
        doStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
      case AsyncWithStmt(ty, is, doStmt) =>
        doStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)   
      case MatchStmt(e, cases) =>
        cases.map(c => c.body.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)).reduceOption(agg).getOrElse(default)
      case TryStmt(tryStmt, handlers, elseStmt, finStmt) =>
        val tryRes: T = tryStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val handRes: T = handlers.map(h => h.body.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)).reduceOption(agg).getOrElse(default) 
        val elseRes: T = tryStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        val finRes: T = finStmt.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
        List(tryRes, handRes, elseRes, finRes).reduceOption(agg).getOrElse(default)
      case OnelineStmt(stmts) =>
        stmts.map(walkStmt(f, agg, default)(_)).reduceOption(agg).getOrElse(default)
      case _ => default 
    }
    agg(curRes, subRes)
  }
  // default walker for Unit type
  def walkStmtUnit(f: Stmt => Unit)(s: Stmt): Unit = walkStmt(f, (_: Unit, _: Unit) => (), ())(s) 
}

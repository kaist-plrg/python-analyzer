package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._

trait Transformer {
  // transformed one AST into another AST
  def apply(ast: Node): Node = ???

  def transform(ast: Node): Node = ast match {
    case Module(body, tyIgnore) => Module(transform(body)(Env())._1, tyIgnore)
  }

  def transform(stmts: List[Stmt])(env: Env): (List[Stmt], Env) = 
    stmts.foldLeft( 
      (List[Stmt](), Env())
    )((pair, stmt) => pair match {
      case (stmtList, env) =>
        val (newStmtList, newEnv) = transform(stmt)(env)
        (stmtList ++ newStmtList, newEnv)
    })

  // TODO refactor divide into List[Stmt] and [Stmt] returning versions
  def transform(stmt: Stmt)(env: Env) : (List[Stmt], Env) = stmt match {
    // function def
    case FunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(FunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    case AsyncFunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(AsyncFunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    // class def
    case ClassDef(decos, name, exprs, kwds, body) =>
      (List(ClassDef(decos, name, exprs, kwds, transform(body)(env)._1)), env)
    // return, del
    case ReturnStmt(eopt) => (List(ReturnStmt(eopt.map(expr => transform(expr)(env)))), env)
    case DelStmt(tl) => (List(DelStmt(tl)), env)
    // strict form of assignment
    case AssignStmt(ts, expr, ty) => ???
    // other form of assignment
    case AugAssign(lhs, bop, rhs) => (List(AugAssign(lhs, bop, transform(rhs)(env))), env)
    case AnnAssign(t, ann, e) => ???
    // for statement
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (List(ForStmt(ty, forExpr, transform(inExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (List(AsyncForStmt(ty, forExpr, transform(inExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    // while statement
    case WhileStmt(wExpr, doStmt, elseStmt) =>
      (List(WhileStmt(transform(wExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env) 
    // if statement
    case IfStmt(cond, thenStmt, elseStmt) =>
      (List(IfStmt(transform(cond)(env), transform(thenStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    // with statement
    case WithStmt(ty, items, doStmt) => ???
    case AsyncWithStmt(ty, items, doStmt) => ???
    // match statement
    case MatchStmt(expr, cases) =>
      (List(MatchStmt(transform(expr)(env), cases.map(c => transform(c)(env)))), env)  
    // exception-related statements
    case RaiseStmt(expr, from) =>
      (List(RaiseStmt(expr, from)), env)
    case TryStmt(tryStmt, handlers, elseStmt, finallyStmt) =>
      val newTryStmt =
        TryStmt(
          transform(tryStmt)(env)._1, handlers.map(handler => transform(handler)(env)),
          transform(elseStmt)(env)._1, transform(finallyStmt)(env)._1)
      (List(newTryStmt), env)
    case AssertStmt(expr, toRaise) =>
      (List(AssertStmt(transform(expr)(env), toRaise)), env)
    // module, scope related statements
    case ImportStmt(al) => ???
    case ImportFromStmt(lv, fromId, al) =>
      (List(ImportFromStmt(lv, fromId, al)), env)
    case GlobalStmt(il) => (List(GlobalStmt(il)), env)
    case NonlocalStmt(il) => (List(NonlocalStmt(il)), env)
  }

  def transform(expr: Expr)(env: Env): Expr = expr match {
    case _ => ???
  }

  def transform(comp: Comprehension)(env: Env): Comprehension = comp match {
    case Compre(target, in, conds) => ??? 
    case AsyncCompre(target, in, conds) => ???
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

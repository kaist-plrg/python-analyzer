package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Stmt extends Node

// Related constructs
trait TyComment // TODO model type comment

case class Kwarg(id: Option[Id], expr: Expr)
case class Alias(name: Id, asName: Option[Id])
case class WithItem(expr: Expr, asExpr: Option[Expr])
case class MatchCase(pattern: Pattern, cond: Option[Expr], body: List[Stmt])

// Match patterns
trait Pattern
case class MatchValue(expr: Expr) extends Pattern
case class MatchSingleton(const: Const) extends Pattern
case class MatchSeq(patterns: List[Pattern]) extends Pattern
case class MatchStar(name: Option[Id]) extends Pattern
case class MatchMapping(map: List[(Expr, Pattern)], name: Option[Id]) extends Pattern
case class MatchClass(classExpr: Expr, patterns: List[Pattern], map: List[(Id, Pattern)]) extends Pattern
case class MatchAs(pattern: Pattern, name: Id) extends Pattern
case class MatchOr(patterns: List[Pattern]) extends Pattern
case object MatchWildcard extends Pattern
case class MatchGroup(pattern: Pattern) extends Pattern

// Exception handler
case class ExcHandler(except: Expr, asName: Option[Id], body: List[Stmt])

// Args
case class Arg(name: Id, ann: Option[Expr], ty: Option[String])
case class Args(posOnlys: List[(Arg, Option[Expr])], normArgs: List[(Arg, Option[Expr])], varArg: Option[Arg], keyOnlys: List[(Arg, Option[Expr])], kwArg: Option[Arg]) 
object Args {
  val empty = Args(Nil, Nil, None, Nil, None)
}

// Comprehension
trait Comp
case class ForComp(target: Expr, in: Expr, cond: List[Expr]) extends Comp
case class AsyncComp(target: Expr, in: Expr, cond: List[Expr]) extends Comp

/////////////////////////////////////////////
// Statements
/////////////////////////////////////////////
// Function, Class definitions
case class FunDef(decos: List[Expr], name: Id, args: Args, retExpr: Option[Expr], tyExpr: Option[String], body: List[Stmt]) extends Stmt
case class AsyncFunDef(decos: List[Expr], name: Id, args: Args, retExpr: Option[Expr], tyExpr: Option[String], body: List[Stmt]) extends Stmt
case class ClassDef(decos: List[Expr], name: Id, exprs: List[Expr], kwds: List[Kwarg], body: List[Stmt]) extends Stmt 

// Simple statements
case class ReturnStmt(expr: Option[Expr]) extends Stmt
case class DelStmt(targets: List[Expr]) extends Stmt
case class AssignStmt(targets: List[Expr], expr: Expr, ty: Option[TyComment]) extends Stmt
case class AugAssign(target: Expr, op: Op, expr: Expr) extends Stmt
case class AnnAssign(target: Expr, ann: Expr, expr: Expr) extends Stmt

// Loops
case class ForStmt(ty: Option[TyComment], forExpr: Expr, inExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class AsyncForStmt(ty: Option[TyComment], forExpr: Expr, inExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class WhileStmt(whileExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class IfStmt(cond: Expr, thenStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt

// With and Pattern matching
case class WithStmt(ty: Option[TyComment], items: List[WithItem], doStmt: List[Stmt]) extends Stmt
case class AsyncWithStmt(ty: Option[TyComment], items: List[WithItem], doStmt: List[Stmt]) extends Stmt
case class MatchStmt(expr: Expr, cases: List[MatchCase]) extends Stmt   

// Exception related
case class RaiseStmt(expr: Option[Expr], from: Option[Expr]) extends Stmt 
case class TryStmt(tryStmt: List[Stmt], handlers: List[ExcHandler], elseStmt: List[Stmt], finallyStmt: List[Stmt]) extends Stmt
case class AssertStmt(expr: Expr, toRaise: Option[Expr]) extends Stmt

// Module, scope related
case class ImportStmt(aliases: List[Alias]) extends Stmt
case class ImportFromStmt(fromId: Option[Id], alises: List[Alias]) extends Stmt
case class GlobalStmt(ids: List[Id]) extends Stmt
case class NonlocalStmt(ids: List[Id]) extends Stmt

// Other simple statements
case class ExprStmt(expr: Expr) extends Stmt
case object PassStmt extends Stmt
case object BreakStmt extends Stmt
case object ContinueStmt extends Stmt

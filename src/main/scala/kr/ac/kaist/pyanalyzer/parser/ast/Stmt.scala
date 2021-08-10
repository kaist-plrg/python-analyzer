package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Stmt extends Node

// Related constructs
trait TyComment // TODO model type comment

case class Alias(name: List[Id], asName: Option[Id]) extends Node
case class WithItem(expr: Expr, asExpr: Option[Expr]) extends Node
case class MatchCase(pattern: Pattern, cond: Option[Expr], body: List[Stmt]) extends Node

// Match patterns
trait Pattern extends Node
case class MatchValue(expr: Expr) extends Pattern
case class MatchSingleton(const: Const) extends Pattern
case class MatchSeq(patterns: List[Pattern]) extends Pattern
case class MatchStar(name: Option[Id]) extends Pattern
case class MatchMapping(
  map: List[(Expr, Pattern)] = Nil,
  name: Option[Id] = None
) extends Pattern
case class MatchClass(
  classExpr: Expr,
  patterns: List[Pattern] = Nil,
  map: List[(Id, Pattern)] = Nil
) extends Pattern
case class MatchAs(pattern: Option[Pattern], name: Id) extends Pattern
case class MatchOr(patterns: List[Pattern]) extends Pattern
case object MatchWildcard extends Pattern
case class MatchGroup(pattern: Pattern) extends Pattern

// Exception handler
case class ExcHandler(
  except: Option[Expr],
  asName: Option[Id],
  body: List[Stmt]
) extends Node

// Args
sealed trait Argument extends Node
case class Args(
  posOnlys: List[(Arg, Option[Expr])] = Nil,
  normArgs: List[(Arg, Option[Expr])] = Nil,
  argSeq: Option[Arg] = None,
  keyOnlys: List[(Arg, Option[Expr])] = Nil,
  kwargMap: Option[Arg] = None
) extends Argument
case class Arg(
  name: Id,
  ann: Option[Expr] = None,
  ty: Option[String] = None
) extends Argument
sealed trait Kwarg extends Argument
case class NormalKwarg(id: Id, expr: Expr) extends Kwarg
case class DoubleStarredKwarg(expr: Expr) extends Kwarg
case class Keyword(arg: Option[Id], value: Expr) extends Kwarg

// Comprehension
sealed trait Comprehension extends Node
case class Compre(
  target: Expr,
  in: Expr,
  conds: List[Expr]
) extends Comprehension
case class AsyncCompre(
  target: Expr,
  in: Expr,
  conds: List[Expr]
) extends Comprehension

/////////////////////////////////////////////
// Statements
/////////////////////////////////////////////
// Function, Class definitions
case class FunDef(decos: List[Expr], name: Id, args: Args, retType: Option[Expr], tyExpr: Option[String], body: List[Stmt]) extends Stmt
case class AsyncFunDef(decos: List[Expr], name: Id, args: Args, retType: Option[Expr], tyExpr: Option[String], body: List[Stmt]) extends Stmt
case class ClassDef(decos: List[Expr], name: Id, exprs: List[Expr], kwds: List[Kwarg], body: List[Stmt]) extends Stmt 

// Simple statements
case class ReturnStmt(expr: Option[Expr]) extends Stmt
case class DelStmt(targets: List[Expr]) extends Stmt
case class AssignStmt(
  targets: List[Expr],
  expr: Expr,
  ty: Option[String] = None
) extends Stmt
case class AugAssign(target: Expr, op: Op, expr: Expr) extends Stmt
case class AnnAssign(target: Expr, ann: Expr, expr: Option[Expr]) extends Stmt

// Loops
case class ForStmt(ty: Option[String], forExpr: Expr, inExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class AsyncForStmt(ty: Option[String], forExpr: Expr, inExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class WhileStmt(whileExpr: Expr, doStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt
case class IfStmt(cond: Expr, thenStmt: List[Stmt], elseStmt: List[Stmt]) extends Stmt

// With and Pattern matching
case class WithStmt(ty: Option[String], items: List[WithItem], doStmt: List[Stmt]) extends Stmt
case class AsyncWithStmt(ty: Option[String], items: List[WithItem], doStmt: List[Stmt]) extends Stmt
case class MatchStmt(expr: Expr, cases: List[MatchCase]) extends Stmt   

// Exception related
case class RaiseStmt(expr: Option[Expr], from: Option[Expr]) extends Stmt 
case class TryStmt(tryStmt: List[Stmt], handlers: List[ExcHandler], elseStmt: List[Stmt], finallyStmt: List[Stmt]) extends Stmt
case class AssertStmt(expr: Expr, toRaise: Option[Expr]) extends Stmt

// Module, scope related
case class ImportStmt(aliases: List[Alias]) extends Stmt
case class ImportFromStmt(level: Int, fromId: List[Id], alises: List[Alias]) extends Stmt
case class GlobalStmt(ids: List[Id]) extends Stmt
case class NonlocalStmt(ids: List[Id]) extends Stmt

// Other simple statements
case class ExprStmt(expr: Expr) extends Stmt
case object PassStmt extends Stmt
case object BreakStmt extends Stmt
case object ContinueStmt extends Stmt

// container for stmt list
case class OnelineStmt(stmts: List[Stmt]) extends Stmt
case class Comment(c: String) extends Stmt

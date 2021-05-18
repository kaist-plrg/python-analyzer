package kr.ac.kaist.pyanalyzer.ast

sealed trait Stmt extends Node

// Simple statements
sealed trait SimpleStmt extends Stmt
// TODO define subs
case class Assignment(id: Name, expr: Expr) extends SimpleStmt
// TODO appropriate modeling of star_expressions and star_expression
case class StarExprs(exprs: List[Expr]) extends SimpleStmt
case class ReturnStmt(exprs: StarExprs) extends SimpleStmt

trait ImportStmt extends SimpleStmt
case class ImportName(ns: List[Name]) extends ImportStmt
// TODO define import_from
case class ImportFrom() extends ImportStmt

trait RaiseStmt extends SimpleStmt
case class RaiseExpr(raise: Expr, from: Expr) extends RaiseStmt
case object RaiseLiteral extends RaiseStmt

case object PassStmt extends SimpleStmt
// TODO define del_targets and subs
case class DelStmt(targets: Node) extends SimpleStmt
case class YieldStmt(expr: Expr) extends SimpleStmt
case class AssertStmt(check: Expr, raise: Expr ) extends SimpleStmt
case object BreakStmt extends SimpleStmt
case object ContinueStmt extends SimpleStmt
case class GlobalStmt(ns: List[Name]) extends SimpleStmt
case class NonlocalStmt(ns: List[Name]) extends SimpleStmt

// Compound statements
sealed trait CompoundStmt extends Stmt

case object FunDefStmt extends CompoundStmt
case object IfStmt extends CompoundStmt
case object ClassDefStmt extends CompoundStmt
case object WithStmt extends CompoundStmt
case object ForStmt extends CompoundStmt
case object TryStmt extends CompoundStmt
case object WhileStmt extends CompoundStmt
case object MatchStmt extends CompoundStmt

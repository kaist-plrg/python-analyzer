package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Stmt extends Node

// Simple statements
sealed trait SimpleStmt extends Stmt
// TODO define subs
case class Assignment(id: Id, expr: Expr) extends SimpleStmt
// TODO appropriate modeling of star_expressions and star_expression
case class StarExprs(exprs: List[Expr]) extends SimpleStmt
case class ReturnStmt(exprs: StarExprs) extends SimpleStmt

trait ImportStmt extends SimpleStmt
case class ImportName(ns: List[String]) extends ImportStmt
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
case class GlobalStmt(ns: List[Id]) extends SimpleStmt
case class NonlocalStmt(ns: List[Id]) extends SimpleStmt

trait Suite

// Compound statements
sealed trait CompoundStmt extends Stmt

case class FunDefStmt(
    deco: List[Expr],
    fname: Id,
    params: List[ParamDec],
    annotation: Expr,
    bodySu: Suite
  ) extends CompoundStmt
case class IfStmt(cond: Expr, thenSu: Suite, elifSu: List[Suite],
  elseSu: Option[Suite]) extends CompoundStmt
case class ClassDefStmt(deco: List[Expr], classname: Id,
  parents: List[Id], bodySu: Suite) extends CompoundStmt
case class WithStmt(items: List[(Expr, Option[Expr])],
  bodySu: Suite) extends CompoundStmt
case class ForStmt(target: List[Expr], list: List[Expr], bodySu: Suite,
  elseSu: Option[Suite]) extends CompoundStmt
case class TryStmt(
    bodySu: Suite,
    exceptionTuple: List[(Option[Expr], Option[Id], Suite)],
    elseSu: Option[Suite],
    finallySu: Option[Suite]
  ) extends CompoundStmt
case class WhileStmt(cond: Expr, bodySu: Suite,
  elseSu: Option[Suite]) extends CompoundStmt
case class MatchStmt(expr: Expr, cases: List[Suite]) extends CompoundStmt
// TODO Coroutine

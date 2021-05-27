package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Stmt extends Node

// Simple statements
sealed trait SimpleStmt extends Stmt
// TODO define subs
case class Assignment(id: AId, expr: Expr) extends SimpleStmt
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
case class GlobalStmt(ns: List[AId]) extends SimpleStmt
case class NonlocalStmt(ns: List[AId]) extends SimpleStmt

trait Suite
trait Param
case class NormalParam(id: AId, pos: Int, default: Some[Value], keyOnly: Boolean) extends Param
// arbitrary positional and keyword args
case class ArbPosParam(id: AId) extends Param
case class ArbKeyParam(id: AId) extends Param

// Compound statements
sealed trait CompoundStmt extends Stmt

case class FunDefStmt(
    deco: List[Expr],
    fname: AId,
    params: List[Param],
    annotation: Expr,
    bodySu: Suite
  ) extends CompoundStmt
case class IfStmt(cond: Expr, thenSu: Suite, elifSu: List[Suite],
  elseSu: Option[Suite]) extends CompoundStmt
case class ClassDefStmt(deco: List[Expr], classname: AId,
  parents: List[AId], bodySu: Suite) extends CompoundStmt
case class WithStmt(items: List[(Expr, Option[Expr])],
  bodySu: Suite) extends CompoundStmt
case class ForStmt(target: List[Expr], list: List[Expr], bodySu: Suite,
  elseSu: Option[Suite]) extends CompoundStmt
case class TryStmt(
    bodySu: Suite,
    exceptionTuple: List[(Option[Expr], Option[AId], Suite)],
    elseSu: Option[Suite],
    finallySu: Option[Suite]
  ) extends CompoundStmt
case class WhileStmt(cond: Expr, bodySu: Suite,
  elseSu: Option[Suite]) extends CompoundStmt
case class MatchStmt(expr: Expr, cases: List[Suite]) extends CompoundStmt
// TODO Coroutine

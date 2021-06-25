package kr.ac.kaist.pyanalyzer.parser.ast

// 6. Expression
sealed trait Expr extends Node
case object EEmpty extends Expr

// Atom: id, literals, and enclosure
case class AId(name: String) extends Expr
case class AStringLiteral(s: String) extends Expr
case class ABytesLiteral(b: String) extends Expr
case class AIntLiteral(i: Int) extends Expr
case class AFloatLiteral(f: Double) extends Expr
case class AImagLiteral(i: Double) extends Expr
case class ABool(b: Boolean) extends Expr
case object ANone extends Expr

// Displays
case class ListExpr(ls: List[Expr]) extends Expr
case class TupleExpr(tup: List[Expr]) extends Expr
case class SetExpr(set: List[Expr]) extends Expr
case class DictExpr(map: List[(Expr, Expr)]) extends Expr

// Primary expressions except atom
case class EAttrRef(prim: Expr, ref: AId) extends Expr
case class ESubscript(prim: Expr, exprs: List[Expr]) extends Expr
case class Call(prim: Expr, args: Args) extends Expr

// Subexpression constructs
// slice : [lb:ub:step]
case class Slice(lb: Option[Expr], ub: Option[Expr], step: Option[Expr]) extends Expr

// function call arguments
// 2 kinds of arguments: positional and keyword
// posRest and keyRest binds extra positional/keyword arguments supplied
// TODO: Refactor Arg
trait Arg extends Node
case class Args(
  posArgs: List[PosArg] = Nil,
  keyArgs: List[KeyArg] = Nil,
  keyStars: List[KeyStar] = Nil,
)
case class PosArg(expr: Expr) extends Arg
case class KeyArg(id: AId, expr: Expr) extends Arg
case class KeyStar(expr: Expr) extends Arg

trait Param extends Node
case class PosParam(id: AId, default: Option[Expr]) extends Param
case class KeyParam(id: AId, default: Option[Expr]) extends Param
// arbitrary positional and keyword args
case class ArbPosParam(id: AId) extends Param
case class ArbKeyParam(id: AId) extends Param

// 6.4-6.13 Arithmetic, Bitwise, Comparison
case class UnaryExpr(op: Op, expr: Expr) extends Expr
case class BinaryExpr(op: Op, lhs: Expr, rhs: Expr) extends Expr
case class CompareExpr(head: Expr, lp: List[(COp, Expr)]) extends Expr
case class AssignExpr(id: AId, expr: Expr) extends Expr
case class CondExpr(ifExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class AwaitExpr(expr: Expr) extends Expr

// 6.14 Lambdas
case class LambdaExpr(parms: List[Param], expr: Expr) extends Expr

// 6.15 stared expression
case class StarExpr(expr: Expr) extends Expr

// Generator, Comprehension related
// TODO understand generator and comprehension
case class CompFor(targets: Expr, inExpr: Expr, ifExpr: List[Expr], async: Boolean) extends Expr
case class ListCompExpr(target: Expr, comp: List[CompFor]) extends Expr
case class SetCompExpr(target: Expr, comp: List[CompFor]) extends Expr
case class DictCompExpr(kv: (Expr, Expr), comp: List[CompFor]) extends Expr

// Generator
case class YieldExpr(opt: Option[Expr]) extends Expr
case class YieldFromExpr(expr: Expr) extends Expr
case class GroupExpr(expr: Expr) extends Expr // TODO understand meaning of this expression,
case class GenExpr(target: Expr, comp: List[Expr]) extends Expr 

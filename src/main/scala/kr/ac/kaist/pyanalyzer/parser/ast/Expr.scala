package kr.ac.kaist.pyanalyzer.parser.ast

///////////////////////////////////
// Expressions
///////////////////////////////////
sealed trait Expr extends Node

// Basic expressions
case class BoolExpr(op: BoolOp, lhs: Expr, rhs: Expr) extends Expr
case class NamedExpr(lhs: Expr, rhs: Expr) extends Expr
case class BinaryExpr(op: BinOp, lhs: Expr, rhs: Expr) extends Expr
case class UnaryExpr(op: UnOp, expr: Expr) extends Expr

// Simple compound expression
case class LambdaExpr(args: Args, expr: Expr) extends Expr
case class IfExpr(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

// Display expressions
case class DictExpr(map: List[(Expr, Expr)]) extends Expr
case class SetExpr(set: List[Expr]) extends Expr
case class ListExpr(ls: List[Expr]) extends Expr
case class TupleExpr(tup: List[Expr]) extends Expr

// Display comprehensions
case class ListComp(target: Expr, comp: List[Comprehension]) extends Expr
case class SetComp(target: Expr, comp: List[Comprehension]) extends Expr
case class DictComp(key: Expr, value: Expr, comp: List[Comprehension]) extends Expr
case class GenComp(expr: Expr, comp:  List[Comprehension]) extends Expr

// Generator related
case class AwaitExpr(expr: Expr) extends Expr
case class YieldExpr(opt: Option[Expr]) extends Expr
case class YieldFromExpr(expr: Expr) extends Expr
case class CompExpr(lhs: Expr, lp: List[(CompOp, Expr)]) extends Expr

// Call
case class Call(fun: Expr, exprs: List[Expr], kwds: List[Keyword]) extends Expr

// Formatted value
case class FormattedValue(lhs: Expr, n: Option[Int], rhs: Option[Expr]) extends Expr

// Other simple expressions
case class JoinedStr(exprs: List[Expr]) extends Expr
case class EConst(c: Constant) extends Expr
case class Attribute(expr: Expr, field: Expr) extends Expr
case class Subscript(expr: Expr, slice: Expr) extends Expr
case class Starred(expr: Expr) extends Expr
case class EName(id: Id) extends Expr
case class Slice(start: Option[Expr], end: Option[Expr], stride: Option[Expr]) extends Expr

case class GroupExpr(expr: Expr) extends Expr

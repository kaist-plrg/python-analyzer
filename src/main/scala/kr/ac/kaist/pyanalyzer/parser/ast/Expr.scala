package kr.ac.kaist.pyanalyzer.parser.ast

// 6. Expression
sealed trait Expr extends Node
case object EEmpty extends Expr

// 6.3 Primaries
trait Primary extends Expr
case class EAttrRef(prim: Primary, ref: AId) extends Primary
case class ESubscript(prim: Primary, exprs: List[Expr]) extends Primary

// TODO model Slice
case class Slice(lb: Expr, ub: Expr, stride: Expr) extends Expr
case class Slicing(prim: Primary, slices: List[Expr]) extends Primary
// TODO argument list is modeled differently in reference. model appropriately
case class Call(prim: Primary, posArgs: List[Expr], keyArgs:Map[AId, Expr], keyRest: Expr) extends Primary

// atoms and literals defined in Atom.scala

// 6.4-6.13 Arithmetic, Bitwise, Comparison
case class PowerExpr(base: Expr, exp: Expr) extends Expr
// TODO distinguish of unary and binary op
case class UnaryExpr(op: Op, expr: Expr) extends Expr
// TODO precedence defined by parsing rule, see 6.17
// possible binary op: *, @, //, /, %, +, -, <<, >>, &, ^, |, <, >, ==, >=, <=, != or, and
case class BinaryExpr(op: Op, lhs: Expr, rhs: Expr) extends Expr
case class AssignExpr(id: AId, expr: Expr) extends Expr
case class CondExpr(ifExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

// 6.14 Lambdas
case class LambdaExpr(parms: List[AId], expr: Expr) extends Expr

// 6.15 stared expression
case class StarExpr(expr: Expr) extends Expr

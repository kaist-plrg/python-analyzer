package kr.ac.kaist.pyanalyzer.parser.ast

// 6. Expression
trait Expr extends Node

// 6.3 Primaries
trait Primary extends Expr
case class AttrRef(prim: Primary, ref: Id) extends Primary
case class Subscript(prim: Primary, exprs: List[Expr]) extends Primary
// TODO model Slice
trait Slice extends Node
case class Slicing(prim: Primary, slices: List[Slice]) extends Primary
// TODO argument list is modeled differently in reference. model appropriately
case class Call(prim: Primary, args: List[Expr]) extends Primary

// 6.2. Atoms
trait Atom extends Primary
case object Identifier extends Atom

trait Literal extends Expr
case class StringLiteral(s: String) extends Literal
case class BytesLiteral(b: Seq[Byte]) extends Literal
case class IntLiteral(n: Int) extends Literal
case class FloatLiteral(f: Double) extends Literal
case class ImagLiteral(j: Double) extends Literal

// 6.4-6.13 Arithmetic, Bitwise, Comparison
case class PowerExpr(base: Expr, exp: UnaryExpr) extends Expr
// TODO distinguish of unary and binary op
case class UnaryExpr(op: Op, expr: UnaryExpr) extends Expr
// TODO precedence defined by parsing rule, see 6.17
// possible binary op: *, @, //, /, %, +, -, <<, >>, &, ^, |, <, >, ==, >=, <=, != or, and
case class BinaryExpr(op: Op, lhs: Expr, rhs: Expr) extends Expr
case class AssignExpr(id: Id, expr: Expr) extends Expr
case class CondExpr(ifExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

// 6.14 Lambdas
case class LambdaExpr(parms: List[Id], expr: Expr) extends Expr

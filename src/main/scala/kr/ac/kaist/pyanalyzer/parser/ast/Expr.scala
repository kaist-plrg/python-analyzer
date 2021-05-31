package kr.ac.kaist.pyanalyzer.parser.ast

// 6. Expression
sealed trait Expr extends Node
case object EEmpty extends Expr

// 6.3 Primaries
trait Primary extends Expr

// 6.2. Atoms
trait Atom extends Primary

// identifiers
case class AId(name: String) extends Atom

// Literals
trait ALiteral extends Atom
case class AStringLiteral(s: String) extends ALiteral
case class ABytesLiteral(b: String) extends ALiteral
case class AIntLiteral(i: Int) extends ALiteral
case class AFloatLiteral(f: Double) extends ALiteral
case class AImagLiteral(i: Double) extends ALiteral
case class ABool(b: Boolean) extends ALiteral
case object ANone extends ALiteral

// Displays
trait Display extends Expr
case class ListDisplay(ls: List[Expr]) extends Display
case class TupleDisplay(tup: List[Expr]) extends Display
case class SetDisplay(set: List[Expr]) extends Display
case class DictDisplay(map: List[(Expr, Expr)], given: List[Expr]) extends Display
case class KVPair(k: Expr, v: Expr) extends Expr

// compound expressions
case class EAttrRef(prim: Expr, ref: AId) extends Primary
case class ESubscript(prim: Expr, exprs: List[Expr]) extends Primary

// TODO model Slice
case class Slice(lb: Expr, ub: Expr, stride: Expr) extends Expr
case class Slicing(prim: Primary, slices: List[Expr]) extends Primary
// TODO argument list is modeled differently in reference. model appropriately
case class Call(prim: Expr, args: Args) extends Primary
case class Args(posArgs: List[PosArg], posRest: List[PosRest], keyArgs: List[KeyArg], keyRest: List[KeyRest])
trait Arg
case class PosArg(expr: Expr) extends Arg
case class PosRest(expr: Expr) extends Arg
case class KeyArg(id: AId, expr: Expr) extends Arg
case class KeyRest(expr: Expr) extends Arg


// atoms and literals defined in Atom.scala

// 6.4-6.13 Arithmetic, Bitwise, Comparison
case class UnaryExpr(op: Op, expr: Expr) extends Expr
case class BinaryExpr(op: Op, lhs: Expr, rhs: Expr) extends Expr
case class AssignExpr(id: AId, expr: Expr) extends Expr
case class CondExpr(ifExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class AwaitExpr(expr: Expr) extends Expr

// 6.14 Lambdas
case class LambdaExpr(parms: List[Param], expr: Expr) extends Expr

// 6.15 stared expression
case class StarExpr(expr: Expr) extends Expr

// Generator, Comprehension related
case class CompExpr(target: List[Expr], inExpr: Expr, ifExpr: List[Expr], async: Boolean) extends Expr
case class YieldExpr(exprList: List[Expr]) extends Expr
case class ListCompExpr(target: Expr, comp: List[Expr]) extends Expr

package kr.ac.kaist.pyanalyzer.parser.ast

///////////////////////////////////
// Expressions
///////////////////////////////////
sealed trait Expr extends Node {
  /*
  ****** precedence help ******

  precedence for each expression. compare it with inner expression
  and conditionally add the parenthesis in Beautifier.

  ex) case UnaryExpr(e) =>
        if this.precedence > e.precedence then
          add parenthesis to explicitly show the order
  In this example, this.precedence is outer precedence,
  and e.precedence is inner precedence.

  Note that outer precedence can be changed frequently in each subexpr.

  ex) case Subscript(prim, slice) =>
        add_parenthesis_conditionally(this.precedence, prim)
        add_parenthesis_conditionally(0 or 1, slice)
          # 0 or 1 is also determined according to another condition

  * default outer precedence is 1
  * default inner(this) precedence is 15
  * this default yeild no parenthesis
  * precedence 0 is special case

  more detail is in cpython/Python/ast_unparse.c

  you can checkout to version without precedence
    024494fce8ad8a57c580ddaeb84ee43ba5fbf311
  which is more easy to understand and extend
  */
  val precedence = this match {
    case e: NamedExpr => 0
    case e: TupleExpr => 0
    case e: IfExpr => 1
    case e: LambdaExpr => 1
    case e: BoolExpr => 2 + e.op.precedence
    case e: CompExpr => 5
    // PR_EXPR
    case e: BinaryExpr => 6 + e.op.precedence
    case e: UnaryExpr => 12 + e.op.precedence
    case e: AwaitExpr => 14
    case _ => 15
  }
}

// Basic expressions
case class BoolExpr(op: BoolOp, lhs: Expr, rhs: Expr) extends Expr
case class NamedExpr(lhs: Expr, rhs: Expr) extends Expr
case class BinaryExpr(op: BinOp, lhs: Expr, rhs: Expr) extends Expr
case class UnaryExpr(op: Op, expr: Expr) extends Expr

// Simple compound expression
case class LambdaExpr(args: Args, expr: Expr) extends Expr
case class IfExpr(expr: Expr, cond: Expr, elseExpr: Expr) extends Expr

// Display expressions
case class DictExpr(map: List[(Expr, Expr)], dstar: List[Expr]) extends Expr
case class SetExpr(set: List[Expr]) extends Expr
case class ListExpr(ls: List[Expr]) extends Expr
case class TupleExpr(tup: List[Expr]) extends Expr

// Display comprehensions
case class ListComp(target: Expr, comp: List[Comprehension]) extends Expr
case class SetComp(target: Expr, comp: List[Comprehension]) extends Expr
case class DictComp(kvpair: (Expr, Expr), comp: List[Comprehension]) extends Expr
case class GenComp(expr: Expr, comp:  List[Comprehension]) extends Expr

// Generator related
case class AwaitExpr(expr: Expr) extends Expr
case class YieldExpr(opt: Option[Expr]) extends Expr
case class YieldFromExpr(expr: Expr) extends Expr
case class CompExpr(lhs: Expr, lp: List[(CompOp, Expr)]) extends Expr

// Call
case class Call(
  fun: Expr,
  exprs: List[Expr] = Nil,
  kwds: List[Kwarg] = Nil
) extends Expr

// Formatted value
case class FormattedValue(lhs: Expr, n: Option[Int], rhs: Option[Expr]) extends Expr

// Other simple expressions
case class JoinedStr(exprs: List[Expr]) extends Expr
case class EConst(c: Const) extends Expr
case class Attribute(expr: Expr, field: Id) extends Expr
case class Subscript(expr: Expr, slice: Expr) extends Expr
case class Starred(expr: Expr) extends Expr
case class DoubleStarred(expr: Expr) extends Expr
case class EName(id: Id) extends Expr
case class Slice(start: Option[Expr], end: Option[Expr], stride: Option[Expr]) extends Expr

case class GroupExpr(expr: Expr) extends Expr

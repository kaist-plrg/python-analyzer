package kr.ac.kaist.pyanalyzer.parser.ast

trait Const extends Node
case object NoneLiteral extends Const
case class IntLiteral(i: Int) extends Const
case class FloatLiteral(f: Double) extends Const
case class ComplexLiteral(c: Double) extends Const
case class StringLiteral(s: String) extends Const
case class BooleanLiteral(b: Boolean) extends Const
case class TupleLiteral(vals: List[Const]) extends Const
case object Ellipsis extends Const

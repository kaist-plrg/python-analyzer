package kr.ac.kaist.pyanalyzer.parser.ast

trait Constant
case object NoneLiteral extends Constant
case class IntLiteral(i: Int) extends Constant
case class FloatLiteral(f: Double) extends Constant
case class ComplexLiteral(r: Double, i: Double) extends Constant
case class StringLiteral(s: String) extends Constant
case class BooleanLiteral(b: Boolean) extends Constant
case class TupleLiteral(vals: List[Constant]) extends Constant
case object Ellipsis extends Constant

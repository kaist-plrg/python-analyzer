package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node

case class AugOp(op: String) extends Op
// TODO: discuss
// case class CompareOp(op: String) extends Op

// binary operators
sealed trait BinOp extends Op
case object OLShift extends BinOp
case object ORShift extends BinOp
case object OAdd extends BinOp
case object OSub extends BinOp
case object OMul extends BinOp
case object ODiv extends BinOp
case object OIDiv extends BinOp
case object OMod extends BinOp
case object OAt extends BinOp
case object OPow extends BinOp
case object OBAnd extends BinOp
case object OBOr extends BinOp
case object OBXor extends BinOp

sealed trait BoolOP extends Op
case object OAnd extends BoolOp
case object OOr extends BoolOp

// compare operators
sealed trait CompOp extends Op
case object CEq extends CompOp
case object CNeq extends CompOp
case object CLte extends CompOp
case object CLt extends CompOp
case object CGte extends CompOp
case object CGt extends CompOp
case object CNotIn extends CompOp
case object CIn extends CompOp
case object CIsNot extends CompOp
case object CIs extends CompOp

// logical operators
sealed trait LOp extends Op
case object LNot extends LOp

sealed trait UnOp extends Op
case object UPlus extends UnOp
case object UMinus extends UnOp
case object UInv extends UnOp // bitwise inverse

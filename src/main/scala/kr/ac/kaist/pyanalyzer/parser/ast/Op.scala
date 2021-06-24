package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node

case class AugOp(op: String) extends Op
// TODO: discuss
// case class CompareOp(op: String) extends Op

// binary operators
sealed trait BOp extends Op
case object OLShift extends BOp
case object ORShift extends BOp
case object OAdd extends BOp
case object OSub extends BOp
case object OMul extends BOp
case object ODiv extends BOp
case object OIDiv extends BOp
case object OMod extends BOp
case object OAt extends BOp
case object OPow extends BOp
case object OBAnd extends BOp
case object OBOr extends BOp
case object OBXor extends BOp

// compare operators
sealed trait COp extends Op
case object CEq extends COp
case object CNeq extends COp
case object CLte extends COp
case object CLt extends COp
case object CGte extends COp
case object CGt extends COp
case object CNotIn extends COp
case object CIn extends COp
case object CIsNot extends COp
case object CIs extends COp

// logical operators
sealed trait LOp extends Op
case object LNot extends LOp
case object LAnd extends LOp
case object LOr extends LOp

sealed trait UOp extends Op
case object UPlus extends UOp
case object UMinus extends UOp
case object UInv extends UOp // bitwise inverse

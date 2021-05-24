package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node

case class AugAssignOp(op: String) extends Op
case class CompareOp(op: String) extends Op

sealed trait BOp extends Op
case object OLShift extends BOp
case object ORShift extends BOp
case object OPlus extends BOp
case object OSub extends BOp
case object OMul extends BOp
case object ODiv extends BOp
case object OIDiv extends BOp
case object OMod extends BOp
case object OMMul extends BOp
case object OPow extends BOp
case object OBAnd extends BOp
case object OBOr extends BOp
case object OBXor extends BOp


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

case object CNot extends COp
case object CAnd extends COp
case object COr extends COp

sealed trait UOp extends Op
case object OUPlus extends UOp
case object OUMinus extends UOp
case object OUInv extends UOp

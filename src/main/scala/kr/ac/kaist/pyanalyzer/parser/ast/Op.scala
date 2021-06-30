package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node

// binary operators
sealed trait BinOp extends Op
case object OLShift extends BinOp // <<
case object ORShift extends BinOp // >>
case object OAdd extends BinOp // +
case object OSub extends BinOp // -
case object OMul extends BinOp // *
case object ODiv extends BinOp // /
case object OIDiv extends BinOp // //
case object OMod extends BinOp // %
case object OAt extends BinOp // @
case object OPow extends BinOp // **
case object OBAnd extends BinOp // &
case object OBOr extends BinOp // |
case object OBXor extends BinOp // ^

// unary operators
sealed trait UnOp extends Op
case object UPlus extends UnOp
case object UMinus extends UnOp
case object UInv extends UnOp // bitwise inverse

sealed trait BoolOP extends Op
case object OAnd extends BoolOp // and
case object OOr extends BoolOp // or

// compare operators
sealed trait CompOp extends Op
case object CEq extends CompOp // ==
case object CNeq extends CompOp // !=
case object CLte extends CompOp // <= 
case object CLt extends CompOp // <
case object CGte extends CompOp // >=
case object CGt extends CompOp // >
case object CNotIn extends CompOp // not in 
case object CIn extends CompOp // in
case object CIsNot extends CompOp // is not
case object CIs extends CompOp // is

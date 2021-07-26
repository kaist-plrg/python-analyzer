package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node {
  val precedence: Int
}

// binary operators
sealed trait BinOp extends Op {
  val precedence = this match {
    case OBOr => 0
    case OBXor => 1
    case OBAnd => 2
    case OLShift => 3
    case ORShift => 3
    case OAdd => 4
    case OSub => 4
    case OPow => 7
    case _ => 5
  }
}
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
sealed trait UnOp extends Op {
  val precedence = this match {
    case UNot => -8
    case _ => 0
  }
}
case object UPlus extends UnOp
case object UMinus extends UnOp
case object UInv extends UnOp // bitwise inverse
case object UNot extends UnOp

sealed trait BoolOp extends Op {
  val precedence = this match {
    case OAnd => 1
    case OOr => 0
  }
}
case object OAnd extends BoolOp // and
case object OOr extends BoolOp // or

// compare operators
sealed trait CompOp extends Op {
  val precedence = 0
}
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

object Op{
  lazy val binOpMap: Map[String, BinOp] = Map(
    "<<" -> OLShift,
    ">>" -> ORShift,
    "+" -> OAdd,
    "-" -> OSub,
    "*" -> OMul,
    "/" -> ODiv,
    "//" -> OIDiv,
    "%" -> OMod,
    "@" -> OAt,
    "**" -> OPow,
    "&" -> OBAnd,
    "|" -> OBOr,
    "^" -> OBXor,
  )
  def getBinOp(s: String): Option[BinOp] = binOpMap get s
}

package kr.ac.kaist.pyanalyzer.parser.ast

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
case object ATrue extends ALiteral
case object AFalse extends ALiteral
case object ANone extends ALiteral

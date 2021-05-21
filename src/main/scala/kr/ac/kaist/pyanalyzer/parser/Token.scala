package kr.ac.kaist.pyanalyzer.parser

// 2. Lexical analysis
abstract class Token(name: String, content: String) {
  override def toString: String = "{" + this.name + "[" + this.content + "]}"
}

case object Newline extends Token("NEWLINE", "\n")
case object Indent extends Token("INDENT", "\t")
case object Dedent extends Token("DEDENT", "\t")
case class Id(s: String) extends Token("id", s)
case class Keyword(s: String) extends Token("keyword", s) 

case class StrLiteral(s: String) extends Token("string literal", s)
case class IntLiteral(i: Int) extends Token("int literal", i.toString)
case class FloatLiteral(f: Double) extends Token("float literal", f.toString)
case class ImagLiteral(i: Double) extends Token("imag literal", i.toString)

case class Op(s: String) extends Token("op", s)
case class Delim(s: String) extends Token("delim", s)

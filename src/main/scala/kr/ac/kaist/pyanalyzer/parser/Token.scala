package kr.ac.kaist.pyanalyzer.parser

// 2. Lexical analysis
abstract class Token(name: String, content: String) {
  override def toString: String = this.content
}

case object Newline extends Token("NEWLINE", "\n")
case object Indent extends Token("INDENT", "\t")
case object Dedent extends Token("DEDENT", "\t")
case class Id(s: String) extends Token("id", s)
case class Keyword(s: String) extends Token("keyword", s) 
case class Literals(s: String) extends Token("literal", s)
case class Op(s: String) extends Token("op", s)
case class Delim(s: String) extends Token("delim", s)

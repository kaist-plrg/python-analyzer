package kr.ac.kaist.pyanalyzer.parser

// 2. Lexical analysis
object Token {
  def toPrettyString(token: Token): String = token match {
    case Newline => "\\n\n"
    case Indent => "INDENT "
    case Dedent => "DEDENT "
    case Id(s) => s"id($s) "
    case Keyword(s) => s + " "
    case StrLiteral(s) => s"lit:$s "
    case BytesLiteral(b) => s"lit:$b "
    case IntLiteral(i) => s"lit:$i "
    case FloatLiteral(f) => s"lit:$f "
    case ImagLiteral(i) => s"lit:${i}j "
    case Op(s) => s + " "
    case Delim(s) => s + " "
  }
  def printTokens(tokens: List[Token]): Unit = tokens.foreach(t => print(toPrettyString(t)))
}
abstract class Token(name: String, content: String) {
  override def toString: String = content
}

case object Newline extends Token("NEWLINE", "\n")
case object Indent extends Token("INDENT", "\t")
case object Dedent extends Token("DEDENT", "\t")
case class Id(s: String) extends Token("id", s)
case class Keyword(s: String) extends Token("keyword", s) 

case class StrLiteral(s: String) extends Token("string literal", s)
case class BytesLiteral(b: String) extends Token("bytes literal", b)
case class IntLiteral(i: Int) extends Token("int literal", i.toString)
case class FloatLiteral(f: Double) extends Token("float literal", f.toString)
case class ImagLiteral(i: Double) extends Token("imag literal", i.toString)

case class Op(s: String) extends Token("op", s)
case class Delim(s: String) extends Token("delim", s)

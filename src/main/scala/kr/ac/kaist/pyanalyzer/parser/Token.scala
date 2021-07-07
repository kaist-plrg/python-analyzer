package kr.ac.kaist.pyanalyzer.parser

// 2. Lexical analysis
object Token {
  def toPrettyString(token: Token): String = token match {
    case NewlineToken(None) => "\\n"
    case NewlineToken(Some(s)) => s"#${s}\\n"
    case IndentToken => "INDENT"
    case DedentToken => "DEDENT"
    case IdToken(s) => s"id($s)"
    case KeywordToken(s) => s
    case StrToken(s) => s""""$s""""
    case BytesToken(b) => s"$b"
    case IntToken(i) => s"$i"
    case FloatToken(f) => s"$f"
    case ImagToken(i) => s"${i}j"
    case OpToken(s) => s
    case DelimToken(s) => s
  }
  def printTokens(tokens: List[Token]): Unit = tokens.foreach(t => print(toPrettyString(t)))
}
abstract class Token(name: String, content: String) {
  override def toString: String = Token.toPrettyString(this)
}

case class NewlineToken(comment: Option[String] = None) extends Token(
  "NEWLINE", s"${if (comment.isEmpty) "" else ("#" + comment.get)}\n"
)
case object IndentToken extends Token("INDENT", "\t")
case object DedentToken extends Token("DEDENT", "\t")
case class IdToken(s: String) extends Token("id", s)
case class KeywordToken(s: String) extends Token("keyword", s) 

case class StrToken(s: String) extends Token("string literal", s)
case class BytesToken(b: String) extends Token("bytes literal", b)
case class IntToken(i: Int) extends Token("int literal", i.toString)
case class FloatToken(f: Double) extends Token("float literal", f.toString)
case class ImagToken(i: Double) extends Token("imag literal", i.toString)

case class OpToken(s: String) extends Token("op", s)
case class DelimToken(s: String) extends Token("delim", s)

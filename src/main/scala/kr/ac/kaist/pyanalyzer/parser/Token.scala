package kr.ac.kaist.pyanalyzer.parser

import scala.Console._
import kr.ac.kaist.pyanalyzer.util.Useful._

// 2. Lexical analysis
object Token {
  def toPrettyString(token: Token): String = token match {
    case NewlineToken(None) => "\\NL\n"
    case NewlineToken(Some(s)) => s"${s} \\NL\n"
    case CommentToken(s) => s"# type:$s"
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

  def toColoredString(token: Token): String = token match {
    case NewlineToken(None) => colored(GRAY)("\\n") + "\n"
    case NewlineToken(Some(s)) => colored(GRAY)(s"#$s \\n") + "\n"
    case CommentToken(s) => colored(GRAY)(s"# type:$s")
    case IndentToken => colored(GRAY)("IN")
    case DedentToken => colored(GRAY)("DE")
    case IdToken(s) => bolded(s"$s")
    case KeywordToken(s) => s
    case StrToken(s) => colored(YELLOW)(s""""$s"""")
    case BytesToken(b) => colored(YELLOW)(s"$b")
    case IntToken(i) => colored(YELLOW)(s"$i")
    case FloatToken(f) => colored(YELLOW)(s"$f")
    case ImagToken(i) => colored(YELLOW)(s"${i}j")
    case OpToken(s) => colored(CYAN)(s)
    case DelimToken(s) => colored(CYAN)(s)
  }

  def printTokens(tokens: List[Token]): Unit = tokens.foreach(t => print(toPrettyString(t)))
  def prettyTokens(tokens: List[Token]): String = tokens.map(t => toPrettyString(t)).mkString(" ")
  def coloredTokens(tokens: List[Token]): String = 
    tokens.map(t => t match {
      case NewlineToken(_) => toColoredString(t)
      case IndentToken => toColoredString(t) + "\n" 
      case DedentToken => toColoredString(t) + "\n"
      case _ => toColoredString(t) + " "
    }).mkString("")
}
abstract class Token(name: String, content: String) {
  //override def toString: String = Token.toPrettyString(this)
}

case class NewlineToken(comment: Option[String] = None) extends Token(
  "NEWLINE", s"${if (comment.isEmpty) "" else ("#" + comment.get)}\n"
)
// only type comments
case class CommentToken(s: String) extends Token("COMMENT", s"# type: $s")
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

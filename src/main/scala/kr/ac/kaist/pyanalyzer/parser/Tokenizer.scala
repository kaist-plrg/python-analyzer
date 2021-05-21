package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.collection.mutable.Stack

case class IndentError(expected: Int, actual: Int) extends Exception
object IndentParser extends IndentParsers (Tokenizer.parseText)
case class IndentParsers(lineParser: String => List[Token]) {
  val stack = Stack[Int]()
  var curIndent = 0 

  def doIndent(n: Int): Unit = { stack.push(n); curIndent += n }
  def doDedent(n: Int): Int = {
    var k = 0
    var sum = 0
    while (sum < n) {
      val indent = stack.pop()
      k += 1
      sum += indent
      if (sum > n) {
        throw IndentError(sum, n) // TODO : precise info for exception
      }
    }
    // only exits loop when sum == n
    curIndent -= sum
    k
  }

  def trimLeft(line: String): String = line.replaceAll("""^\s+""", "")
  def getIndent(line: String): Int = line.length - trimLeft(line).length
  
  def parseLine(line: String): List[Token] = {
    val trimmed = trimLeft(line)
    val newIndent = getIndent(line) 
    if (newIndent > curIndent) {
      val delta = newIndent - curIndent
      doIndent(delta) 
      Indent +: lineParser(trimmed)
    } else if (newIndent < curIndent) {
      val delta = curIndent - newIndent
      val count = doDedent(delta)
      List.fill(count)(Dedent) ++ lineParser(trimmed) 
    } else {
      lineParser(trimmed)
    }
  }

  def parseLines(lines: List[String]): List[Token] =
    lines.flatMap(line => parseLine(line) :+ Newline)

  def parse(text: String): List[Token] = parseLines(text.split("\n").toList)
}

// TODO change to Parser[Token]
object Tokenizer extends Tokenizers
trait Tokenizers extends RegexParsers {
  // line, comments, indents, whitespaces
  lazy val line = ".*\n".r
  lazy val comments = "#.*\n".r
  // TODO implicit line joining
  lazy val whitespace = """\s""".r //TODO just use \s in needed
  // TODO stack-based indent parser needed: look at 2.1.8 last line
  
  // identifier
  lazy val id_start = """\D""".r
  lazy val id_continue = """\w*""".r
  lazy val identifier: Parser[Id] = (id_start ~ id_continue) ^^ {
    case st ~ cts => Id(st + cts.mkString(""))
  }

  // keywords
  lazy val keyword = List(
    "False", "None", "True", "and", "as", "assert", "async",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield",
  ).mkString("|").r ^^ { case s => Keyword(s)}

  // literals
  lazy val quote = "['\"]".r
  lazy val tripleQuote = "['\"]{3}".r
  lazy val shortQuote = quote ~> "[^'\"]*".r <~ quote
  lazy val longQuote = tripleQuote ~> "[^'\"]*".r <~ tripleQuote
  lazy val stringPrefix = "r".r | "u".r | "R".r | "U".r | "f".r | "F".r |
    "fr".r | "Fr".r | "fR".r | "rf".r | "rF".r | "Rf".r | "RF".r
  lazy val stringLiteral: Parser[StrLiteral] = opt(stringPrefix) ~>
    (longQuote | shortQuote) ^^ StrLiteral
  lazy val bytesPrefix = "b".r | "B".r | "br".r | "bR".r | "BR".r |
    "rb".r | "rB".r | "Rb".r | "RB".r
  lazy val bytesLiteral: Parser[BytesLiteral] = bytesPrefix ~>
    (longQuote | shortQuote) ^^ BytesLiteral

  // TODO escape character how?

  // TODO bytesLiteral
  // TODO format string
  lazy val digit = "[0-9]".r
  lazy val nonZeroDigit = "[1-9]".r
  lazy val decInteger: Parser[(String, Int)] = "[0-9]+".r ^^ { (_, 10) } 
  //lazy val decInteger: Parser[(String, Int)] = """[([1-9](_\d)*)(0+(_?0)*)]""".r ^^ { (_, 10) }
  lazy val binInteger: Parser[(String, Int)] = "[(0b)(0B)](_?[0-1])+".r ^^ { (_, 2) }
  lazy val octInteger: Parser[(String, Int)] = "[(0o)(0O)](_?[0-7])+".r ^^ { (_, 8) }
  lazy val hexInteger: Parser[(String, Int)] = "[(0x)(0X)](_?[0-7[a-f][A-F]])+".r ^^ { (_, 16) }
  lazy val integer: Parser[IntLiteral] = (decInteger | binInteger | octInteger | hexInteger) ^^ {
    case (s, b) => IntLiteral(Integer.parseInt(s, b)) 
  }

  lazy val digitPart = """\d(_?\d)*""".r
  lazy val fraction = ".".r ~ digitPart
  lazy val exponent = "[eE][+-]".r ~ digitPart
  lazy val pointFloat = digitPart.? ~ fraction | digitPart ~ ".".r
  lazy val exponentFloat = (digitPart | pointFloat) ~ exponent
  lazy val floatNumber: Parser[FloatLiteral] = (pointFloat | exponentFloat) ^^ { 
    case s => FloatLiteral(s.toString.toDouble) // TODO why type error when no toString? 
  }

  lazy val imagNumber: Parser[ImagLiteral] = ((floatNumber | digitPart) <~ "[jJ]".r) ^^ {
    case s => ImagLiteral(s.toString.toDouble)
  }

  // operator
  lazy val op = List(
    "\\+", "-", "\\*", "\\*\\*", "/", "//", "%", "@",
    "<<", ">>", "&", "\\|", "\\^", "~",
    "<", ">", "<=", ">=", "==", "!=",
  ).mkString("|").r ^^ {
    case s => Op(s)
  }

  // some operator contians delimiters, so need to be tokenized first
  lazy val opBeforeDelim = List(
    ":="
  ).mkString("|").r ^^ {
    case s => Op(s)
  }

  // delimiter
  lazy val delim = List(
    "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", 
    ",", ":", "\\.", ";", "@", "=", "->",
    "\\+=", "-=", "\\*=", "/=", "//=", "%=", "@=",
    "&=", "\\|=", "\\^=", ">>=", "<<=", "\\*\\*=",
  ).mkString("|").r ^^ { case s => Delim(s) }

  // parseAll
  lazy val literal: Parser[Token] = integer | floatNumber | imagNumber | stringLiteral
  lazy val token: Parser[Token] = opBeforeDelim | delim | op | keyword | literal | identifier
  lazy val tokens: Parser[List[Token]] = rep(token)
  def parseText(input: String): List[Token] = parseAll(tokens, input).get
}

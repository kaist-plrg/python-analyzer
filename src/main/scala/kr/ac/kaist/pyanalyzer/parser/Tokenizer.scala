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
    // val newIndent = getIndent(line) 
    // if (newIndent > curIndent) {
    //   val delta = newIndent - curIndent
    //   doIndent(delta) 
    //   Indent +: lineParser(trimmed)
    // } else if (newIndent < curIndent) {
    //   val delta = curIndent - newIndent
    //   val count = doDedent(delta)
    //   List.fill(count)(Dedent) ++ lineParser(trimmed) 
    // } else {
    //   lineParser(trimmed)
    // }
    lineParser(trimmed)
  }

  def parseLines(lines: List[String]): List[Token] =
    lines.flatMap(line => parseLine(line) :+ Newline)

  def parse(text: String): List[Token] = parseLines(text.split("\n").toList)
}

// TODO change to Parser[Token]
object Tokenizer extends Tokenizers
trait Tokenizers extends RegexParsers {
  override def skipWhitespace = false
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
  val keywords = List(
    "False", "None", "True", "and", "as", "assert", "async",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield",
  )
  lazy val keyword = keywords.mkString("|").r ^^ { case s => Keyword(s)}

  // literals
  lazy val quote = "['\"]".r
  lazy val tripleQuote = "['\"]{3}".r
  lazy val shortQuote = quote ~> "[^'\"]*".r <~ quote
  lazy val longQuote = tripleQuote ~> "[^'\"]*".r <~ tripleQuote
  lazy val stringPrefix = List("fr", "Fr", "fR", "FR", "rf", "rF", "Rf",
    "RF", "r", "u", "R", "U", "f", "F").mkString("|").r
  lazy val stringLiteral: Parser[StrLiteral] = opt(stringPrefix) ~>
    (longQuote | shortQuote) ^^ StrLiteral
  lazy val bytesPrefix = List("br", "Br", "bR", "BR","rb", "rB", "Rb",
    "RB", "b", "B").mkString("|").r
  lazy val bytesLiteral: Parser[BytesLiteral] = bytesPrefix ~>
    (longQuote | shortQuote) ^^ BytesLiteral

  // TODO escape character how?

  // TODO format string
  lazy val digit = "[0-9]".r
  lazy val nonZeroDigit = "[1-9]".r
  lazy val decInteger: Parser[(String, Int)] = "[0-9]+".r ^^ { (_, 10) } 
  //lazy val decInteger: Parser[(String, Int)] = """[([1-9](_\d)*)(0+(_?0)*)]""".r ^^ { (_, 10) }
  lazy val binInteger: Parser[(String, Int)] = "((0b)|(0B))(_?[0-1])+".r ^^ { (_, 2) }
  lazy val octInteger: Parser[(String, Int)] = "((0o)|(0O))(_?[0-7])+".r ^^ { (_, 8) }
  lazy val hexInteger: Parser[(String, Int)] = "((0x)|(0X))(_?[0-7[a-f][A-F]])+".r ^^ { (_, 16) }
  lazy val integer: Parser[IntLiteral] = (decInteger | binInteger | octInteger | hexInteger) ^^ {
    case (s, b) => IntLiteral(Integer.parseInt(s, b)) 
  }

  def comp2str(comp: ~[String, String]): String = {
    val ~(lhs, rhs) = comp
    lhs + rhs
  }

  implicit lazy val temp = (p: Parser[String ~ String]) => p ^^ { comp2str(_) }

  lazy val digitPart = digit ~ rep(opt("_".r) ~> digit) ^^ {
    case d ~ ld =>  ld.foldLeft(d)((res, e) => res + e)
  }
  lazy val fraction: Parser[String] = "[.]".r ~ digitPart
  lazy val exponent = "[eE]".r ~ opt("[+-]".r) ~ digitPart ^^ {
    case e ~ Some("-") ~ d => s"$e-$d"
    case e ~ _ ~ d => s"$e$d"
  }
  lazy val pointFloat: Parser[String] = opt(digitPart) ~ fraction ^^ {
    case opt ~ f => opt.getOrElse("0") + f
  } | digitPart ~ "[.]".r
  lazy val exponentFloat: Parser[String] = (pointFloat | digitPart) ~ exponent
  lazy val floatNumber: Parser[FloatLiteral] = (exponentFloat | pointFloat) ^^ { 
    case s => FloatLiteral(s.toDouble)
  }

  lazy val imagNumber: Parser[ImagLiteral] = ((exponentFloat | pointFloat) | digitPart) <~ "[jJ]".r ^^ {
    case s => ImagLiteral(s.toDouble)
  }

  // operator
  lazy val op = List(
    "\\+", "-", "\\*\\*", "\\*", "//", "/", "%", "@",
    "<<", ">>", "&", "\\|", "\\^", "~",
    "<=", ">=", "<", ">", "!=",
  ).mkString("|").r ^^ {
    case s => Op(s)
  }

  // some operator contians delimiters, so need to be tokenized first
  lazy val opBeforeDelim = List(
    ":=", "==", 
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
  lazy val literal: Parser[Token] = imagNumber | floatNumber | integer | stringLiteral | bytesLiteral
  lazy val token: Parser[Token] = literal | opBeforeDelim | delim | op | keyword | identifier
  lazy val tokens: Parser[List[Token]] = rep(token)
  def parseText(input: String): List[Token] = parseAll(tokens, input).get
}

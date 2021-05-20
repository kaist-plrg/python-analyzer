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
    while (sum <= n) {
      val indent = stack.pop
      k += 1
      sum += indent
      if (sum > n) {
        throw IndentError(sum, n) // TODO : precise info for exception
      }
    }
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
      lineParser(trimmed) :+ Indent
    } else if (newIndent < curIndent) {
      val delta = curIndent - newIndent
      val count = doDedent(delta)
      lineParser(trimmed) ++ List.fill(count)(Dedent)
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
  lazy val id_continue = ".".r
  lazy val identifier: Parser[Id] = (id_start ~ id_continue.*) ^^ {
    case st ~ cts => Id(st + cts.mkString(""))
  }


  // keywords
  // lazy val keywords = "False None True and as assert async await break class continue def del elif else except finally for from global if import in is lambda nonlocal not or pass raise return try while with yield".split(" ").map(_.r).reduce(_ | _)

  // literals
  lazy val shortStrCode = "[\"\']".r
  lazy val longStrCode = "[(\"\"\")(\'\'\')]".r
  lazy val shortString = shortStrCode ~> ".*".r <~ shortStrCode
  lazy val longString = longStrCode ~> ".*".r <~ longStrCode
  lazy val stringLiteral: Parser[StrLiteral] = (shortString | longString) ^^ {
    case s => StrLiteral(s)
  }

  // TODO escape character how?

  // TODO bytesLiteral
  // TODO format string
  lazy val decInteger: Parser[(String, Int)] = """[([1-9](_\d)*)(0+(_?0)*)]""".r ^^ { (_, 10) }
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

  // Operators
  /*lazy val operator: Parser[Op] = """[+-*(**)/(//)%@(<<)(>>)&\|^~(:=)<>(<=)(>=)(==)(!=)]""".r ^^ {
    case s => Op(s)
  }*/

  // parseAll
  lazy val token: Parser[Token] = identifier | integer | floatNumber | imagNumber 
  lazy val tokens: Parser[List[Token]] = rep(token)
  def parseText(input: String): List[Token] = parseAll(tokens, input).get
}

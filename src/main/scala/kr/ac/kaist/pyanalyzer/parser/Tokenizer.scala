package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._

// TODO change to Parser[Token]
trait Tokenizer extends RegexParsers {
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
  lazy val floatNumber = pointFloat | exponentFloat

  lazy val imagNumber = (floatNumber | digitPart) ~ "[jJ]".r

  // Operators

}

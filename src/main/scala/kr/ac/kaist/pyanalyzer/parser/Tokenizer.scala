package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.collection.mutable.Stack

case class IndentError(expected: Int, actual: Int) extends Exception
case class IndentState(tabs: List[Int], cur: Int) {
  def doIndent(n: Int): IndentState = IndentState(n :: tabs, cur + n) 
  def doDedent(n: Int): (Int, IndentState) = {
    var k = 0
    var sum = 0
    while (sum < n){
      val indent = tabs(k)
      k += 1
      sum += indent
      if (sum > n) {
        throw IndentError(sum, n)
      }
    }
    // only breaks loop when sum == n
    (k, IndentState(tabs.drop(k), cur - sum))
  }
}

// lineParser: a parser that parses one line string, without considering indentation
// IndentParser will take care of indent, dedent, and newline
case class IndentParser(st: IndentState) {
  // helper
  def leftTrim(s: String) = s.replaceAll("^\\s+", "")
  val onelineParser = (Tokenizer.parseText _) 

  // uses current state to parse a line into tokens, and produce a new state
  def parseLine(line: String): (List[Token], IndentState) = {
    // get current line's indentation 
    val trimmed = leftTrim(line)
    val newIndent = line.length() - trimmed.length()
    
    // try parsing a line
    // indent case
    if (st.cur < newIndent) {
      val newSt = st.doIndent(newIndent)
      (Indent +: onelineParser(trimmed) :+ Newline, newSt) 
    }
    // dedent case
    else if (st.cur > newIndent) {
      val (k, newSt) = st.doDedent(newIndent)
      (Dedent +: onelineParser(trimmed) :+ Newline, newSt)
    }
    // same case
    else {
      (onelineParser(trimmed) :+ Newline, st)      
    }
  }

  // applies parseLine to all lines, maintaining the state 
  def apply(text: String): List[Token] = {
    val lines = text.split("\n")
    var state = st
    var tokens = List[Token]()

    for (line <- lines) {
      val (lineToks, newSt) = parseLine(line)
      state = newSt
      tokens = tokens ++ lineToks
    }

    tokens
  }
}

case class IndentParsers(lineParser: String => List[Token]) {
  ///////// Re-implementing
  def apply(text: String): List[Token] = {
    val lines = text.split("\n")
    val tokens = ???
    tokens
  }
}

// TODO change to Parser[Token]
object Tokenizer extends Tokenizers
trait Tokenizers extends RegexParsers {
  // line, comments, indents, whitespaces
  lazy val line = ".*\n".r
  lazy val comments = "#[^\n]*".r
  // TODO implicit line joining
  lazy val whitespace = """[ \f\t]*""".r //TODO just use \s in needed
  
  // identifier 
  // lazy val id_start = log("""\w""".r)("idstart")
  // lazy val id_continue = log("""\w""".r)("idcont")
  // one token - one regex object, or whitespace is captured inbetween
  // TODO Unicode characters are being ignored
  lazy val identifier: Parser[Id] = """([a-zA-Z_])([a-zA-Z_0-9])*""".r ^^ Id

  // keywords
  val keywords = List(
    "False", "None", "True", "and", "async", "as", "assert",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield",
  )
  lazy val keyword = keywords.mkString("|").r ^^ { case s => Keyword(s) }

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

  /*
  lazy val digitPart = digit ~ rep(opt("_".r) ~> digit) ^^ {
    case d ~ ld =>  ld.foldLeft(d)((res, e) => res + e)
  }
  lazy val fraction: Parser[String] = "[.]".r ~ digitPart
  lazy val exponent = "[eE]".r ~ opt("[+-]".r) ~ digitPart ^^ {
    case e ~ Some("-") ~ d => s"$e-$d"
    case e ~ _ ~ d => s"$e$d"
  }
  */
  /* lazy val pointFloat: Parser[String] = opt(digitPart) ~ fraction ^^ {
    case opt ~ f => opt.getOrElse("0") + f
  } | digitPart ~ "[.]".r */
  /*
  lazy val pointFloat: Parser[String] = """(\d(_?\d)*\.\d(_?\d)*)|(\d(_?\d)*\.)""".r ^^ {
    case s => s.replaceAll("_", "")
  }
  lazy val exponentFloat: Parser[String] = (pointFloat | digitPart) ~ exponent
  lazy val floatNumber: Parser[FloatLiteral] = (exponentFloat | pointFloat) ^^ { 
    case s => FloatLiteral(s.toDouble)
  }
  */

  lazy val imagNumber: Parser[ImagLiteral] = imagNumberRegex.r <~ "[jJ]".r ^^ {
    case s => ImagLiteral(s.toDouble)
  }
  lazy val imagNumberRegex = "(" + floatNumberRegex + "|" + digitPart + ")"
  // refactoring regex: intermediate regex is string, only top-level matchers are Parser[]
  lazy val digitPart = """(\d(_?\d)*)"""
  lazy val fraction = "(" + """\.""" + digitPart + ")"
  lazy val exponent = "(" + """[eE][+-]?""" + digitPart + ")"
  lazy val pointFloat = "(" + "(" + digitPart + "?" + fraction + ")" + "|" + "(" + digitPart + """\.""" + ")" + ")"   
  lazy val exponentFloat = "(" + "(" + digitPart + "|" + pointFloat + ")" + exponent + ")"
  lazy val floatNumberRegex = "(" + exponentFloat + "|" + pointFloat + ")"
  lazy val floatNumber: Parser[FloatLiteral] = floatNumberRegex.r ^^ {
    case s => FloatLiteral(s.replaceAll("_", "").toDouble) 
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
  lazy val token: Parser[Token] = (literal | opBeforeDelim | delim | op | keyword | identifier)
  lazy val tokens: Parser[List[Token]] = rep(token)
  def parseText(input: String): List[Token] = parseAll(tokens, input).get
}

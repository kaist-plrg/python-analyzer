package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.collection.mutable.Stack
import kr.ac.kaist.pyanalyzer.util.Useful._

case class IndentError(expected: Int, actual: Int) extends Exception
case class IndentState(tabs: List[Int], cur: Int) {
  // expects positive int
  def doIndent(n: Int): IndentState = IndentState(n:: tabs, cur + n) 
  // expects negative int
  def doDedent(n: Int): (Int, IndentState) = {
    val delta = -n
    var k = 0
    var sum = 0
    while (sum < delta){
      val indent = tabs(k)
      k += 1
      sum += indent
      if (sum > delta) {
        throw IndentError(sum, delta)
      }
    }
    // only breaks loop when sum == n
    (k, IndentState(tabs.drop(k), cur - sum))
  }
}

// lineParser: a parser that parses one line string, without considering indentation
// IndentParser will take care of indent, dedent, and newline
case class IndentParser(var st: IndentState) {
  // helper
  def leftTrim(s: String) = s.replaceAll("^\\s+", "")
  val onelineParser = (Tokenizer.parseText _) 

  // uses current state to parse a line into tokens, and produce a new state
  def parseLine(line: String): (List[Token], IndentState) = {
    // get current line's indentation 
    val trimmed = leftTrim(line)
    val newIndent = line.length() - trimmed.length()
    val parsed: List[Token] = onelineParser(trimmed)

    // TODO add this in test 
    // ASSERT parse result must be nonempty, and ends with NewlineToken
    assert(parsed.length > 0)("line parse result must be nonempty")
    assert(parsed.last match {
      case NewlineToken(_) => true
      case _ => false
    })("last token must be Newline")

    // if only NewlineToken or CommentToken returned, don't consider the indent. (no state update)
    // TODO refactor this functionally
    parsed match {
      // Blank NewlineToken: safely ignore
      case NewlineToken(None) :: Nil => return (Nil, st)
      case CommentToken(_) :: NewlineToken(_) :: Nil => return (parsed, st)
      case t :: Nil => return (parsed, st)
      case _ => 
    }

    // indent case
    val delta =  newIndent - st.cur
    if (delta > 0) {
      val newSt = st.doIndent(delta)
      (IndentToken +: parsed, newSt) 
    }
    // dedent case
    else if (delta < 0) {
      val (k, newSt) = st.doDedent(delta)
      val dedents = List.fill(k)(DedentToken)
      (dedents ++ parsed, newSt)
    }
    // same case
    else {
      (parsed, st)      
    }
  }

  // flushes remaining dedent tokens 
  def flush: List[Token] = st.tabs.map(_ => DedentToken)

  // applies parseLine to all lines, maintaining the state 
  def apply(text: String): List[Token] = {
    val lines = text.split("\n")
    var state = st
    var tokens = List[Token]()

    for (line <- lines) {
      val (lineToks, newSt) = parseLine(line)
      this.st = newSt
      tokens = tokens ++ lineToks
    }

    tokens ++ this.flush
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
  // lazy val line = ".*\n".r
  lazy val comment = "#[^\n]*".r
  // TODO implicit line joining
  lazy val whitespace = """[ \f\t]*""".r //TODO just use \s in needed
  
  // identifier 
  // lazy val id_start = log("""\w""".r)("idstart")
  // lazy val id_continue = log("""\w""".r)("idcont")
  // one token - one regex object, or whitespace is captured inbetween
  // TODO Unicode characters are being ignored
  lazy val identifier: Parser[IdToken] = """([a-zA-Z_])([a-zA-Z_0-9])*""".r ^^ IdToken

  // keywords
  val keywords = List(
    "False", "None", "True", "and", "async", "assert", "as",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield",
  )
  lazy val keyword = keywords.mkString("|").r ^^ { case s => KeywordToken(s) }

  // literals
  lazy val quote = "['\"]".r
  lazy val tripleQuote = "['\"]{3}".r
  lazy val shortQuote = quote ~> "[^'\"]*".r <~ quote
  lazy val longQuote = tripleQuote ~> "[^'\"]*".r <~ tripleQuote
  lazy val stringPrefix = List("fr", "Fr", "fR", "FR", "rf", "rF", "Rf",
    "RF", "r", "u", "R", "U", "f", "F").mkString("|").r
  lazy val stringLiteral: Parser[StrToken] = opt(stringPrefix) ~>
    (longQuote | shortQuote) ^^ StrToken
  lazy val bytesPrefix = List("br", "Br", "bR", "BR","rb", "rB", "Rb",
    "RB", "b", "B").mkString("|").r
  lazy val bytesLiteral: Parser[BytesToken] = bytesPrefix ~>
    (longQuote | shortQuote) ^^ BytesToken

  // TODO format string
  lazy val digit = "[0-9]".r
  lazy val nonZeroDigit = "[1-9]".r
  lazy val decInteger: Parser[(String, Int)] = "[0-9]+".r ^^ { (_, 10) } 
  //lazy val decInteger: Parser[(String, Int)] = """[([1-9](_\d)*)(0+(_?0)*)]""".r ^^ { (_, 10) }
  lazy val binInteger: Parser[(String, Int)] = "((0b)|(0B))(_?[0-1])+".r ^^ { (_, 2) }
  lazy val octInteger: Parser[(String, Int)] = "((0o)|(0O))(_?[0-7])+".r ^^ { (_, 8) }
  lazy val hexInteger: Parser[(String, Int)] = "((0x)|(0X))(_?[0-7[a-f][A-F]])+".r ^^ { (_, 16) }
  lazy val integer: Parser[IntToken] = (decInteger | binInteger | octInteger | hexInteger) ^^ {
    case (s, b) => IntToken(Integer.parseInt(s, b)) 
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
  lazy val floatNumber: Parser[FloatToken] = (exponentFloat | pointFloat) ^^ { 
    case s => FloatToken(s.toDouble)
  }
  */

  lazy val imagNumber: Parser[ImagToken] = imagNumberRegex.r <~ "[jJ]".r ^^ {
    case s => ImagToken(s.toDouble)
  }
  lazy val imagNumberRegex = "(" + floatNumberRegex + "|" + digitPart + ")"
  // refactoring regex: intermediate regex is string, only top-level matchers are Parser[]
  lazy val digitPart = """(\d(_?\d)*)"""
  lazy val fraction = "(" + """\.""" + digitPart + ")"
  lazy val exponent = "(" + """[eE][+-]?""" + digitPart + ")"
  lazy val pointFloat = "(" + "(" + digitPart + "?" + fraction + ")" + "|" + "(" + digitPart + """\.""" + ")" + ")"   
  lazy val exponentFloat = "(" + "(" + digitPart + "|" + pointFloat + ")" + exponent + ")"
  lazy val floatNumberRegex = "(" + exponentFloat + "|" + pointFloat + ")"
  lazy val floatNumber: Parser[FloatToken] = floatNumberRegex.r ^^ {
    case s => FloatToken(s.replaceAll("_", "").toDouble) 
  }

  // operator
  lazy val op = List(
    "\\+", "-", "\\*\\*", "\\*", "//", "/", "%", "@",
    "<<", ">>", "&", "\\|", "\\^", "~",
    "<=", ">=", "<", ">", "!=",
  ).mkString("|").r ^^ {
    case s => OpToken(s)
  }

  // some operator contians delimiters, so need to be tokenized first
  lazy val opBeforeDelim = List(
    ":=", "==", 
  ).mkString("|").r ^^ {
    case s => OpToken(s)
  }

  // delimiter
  lazy val delim = List(
    "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", 
    ",", ":", "\\.", ";", "@", "=", "->",
    "\\+=", "-=", "\\*=", "/=", "//=", "%=", "@=",
    "&=", "\\|=", "\\^=", ">>=", "<<=", "\\*\\*=",
  ).mkString("|").r ^^ { case s => DelimToken(s) }

  // top level
  lazy val literal: Parser[Token] = imagNumber | floatNumber | integer | stringLiteral | bytesLiteral
  lazy val token: Parser[Token] = (literal | opBeforeDelim | delim | op | (keyword ||| identifier))
  lazy val tokens: Parser[List[Token]] = rep(token)

  // line tokenizer considering comment and newline
  lazy val line: Parser[List[Token]] = tokens ~ opt(comment) ^^ {
    case tl ~ Some(s) if s.startsWith("# type:") => 
      tl ++ List(CommentToken(s.replaceFirst("# type:", "")), NewlineToken())
    case tl ~ copt => tl :+ NewlineToken(copt)
  }

  // parseAll
  def parseText(input: String): List[Token] = parseAll(line, input).get
}

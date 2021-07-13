package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.collection.mutable.Stack
import kr.ac.kaist.pyanalyzer.util.Useful._
import org.apache.commons.text.StringEscapeUtils._

// TODO change to Parser[Token]
object Tokenizer extends Tokenizers {
  def tokenizeText(source: String): List[Token] =
    IndentParser()(source)
}

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
  lazy val keyword = keywords.mkString("|").r ^^ KeywordToken

  // literals
  lazy val shortSingleQuote = "'".r ~> "([^\\\\']|\\\\.)*".r <~ "'".r
  lazy val shortDoubleQuote = "\"".r ~> "([^\\\\\"]|\\\\.)*".r <~ "\"".r
  lazy val shortQuote = shortSingleQuote | shortDoubleQuote
  lazy val longSingleQuote = "'''".r ~> "([^\\\\]|\\\\.)*(?=''')".r <~ "'''".r
  lazy val longDoubleQuote = "\"\"\"".r ~> "([^\\\\]|\\\\.)*(?=\"\"\")".r <~ "\"\"\"".r
  lazy val longQuote = longSingleQuote | longDoubleQuote
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
  ).mkString("|").r ^^ OpToken

  // some operator contians delimiters, so need to be tokenized first
  lazy val opBeforeDelim = List(
    ":=", "==", 
  ).mkString("|").r ^^ OpToken

  // delimiter
  lazy val delim = List(
    "\\(", "\\)", "\\[", "\\]", "\\{", "\\}", 
    ",", ":", "\\.\\.\\.", "\\.", ";", "@", "=", "->",
    "\\+=", "-=", "\\*=", "/=", "//=", "%=", "@=",
    "&=", "\\|=", "\\^=", ">>=", "<<=", "\\*\\*=",
  ).mkString("|").r ^^ DelimToken

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

case class IndentError(expected: Int, actual: Int) extends Exception
case class DelimError(expected: String, actual: String) extends Exception
case class IndentState(
  tabs: List[Int] = Nil,
  cur: Int = 0,
  delims: List[String] = Nil,
) {
  // expects positive int
  def doIndent(n: Int): IndentState = IndentState(n :: tabs, cur + n)
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
  // maintaining parenthesis state
  // only "(", "[", or "{" should be pushed
  def pushDelim(s: String): IndentState = IndentState(tabs, cur, s :: delims)
  def popDelim(s: String): IndentState = {
    if (delims.isEmpty) throw DelimError("", s)
    delims.head match {
      case "(" =>
        if (s == ")") IndentState(tabs, cur, delims.drop(1)) else throw DelimError(")", s)
      case "[" =>
        if (s == "]") IndentState(tabs, cur, delims.drop(1)) else throw DelimError("]", s)
      case "{" =>
        if (s == "}") IndentState(tabs, cur, delims.drop(1)) else throw DelimError("}", s)
      case _ => ???
    }
  }
}

// lineParser: a parser that parses one line string, without considering indentation
// IndentParser will take care of indent, dedent, and newline
case class IndentParser(var st: IndentState = IndentState()) {
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
      case NewlineToken(_) :: Nil => return (Nil, st)
      case CommentToken(_) :: NewlineToken(_) :: Nil => return (Nil, st)
      case t :: Nil => return (parsed, st)
      case _ =>
    }
    
    // Processing for implicit line joining
    // scan the tokenlist, count the parenthesis state
    val newDelimSt = parsed.foldLeft(st)((st, tok) => tok match {
      case DelimToken("(") => st.pushDelim("(")
      case DelimToken("[") => st.pushDelim("[")
      case DelimToken("{") => st.pushDelim("{")
      case DelimToken(")") => st.popDelim(")")
      case DelimToken("]") => st.popDelim("]")
      case DelimToken("}") => st.popDelim("}")
      case _ => st
    })
    // if current state is already implicit line joining, do it
    // ie. newline token at the last should be eliminated
    if (st.delims.nonEmpty) {
      if (newDelimSt.delims.nonEmpty) {
        return (parsed.dropRight(1), newDelimSt)
      }
      else {
        return (parsed, newDelimSt)
      }
    }
    if (newDelimSt.delims.nonEmpty) {
      return (parsed.dropRight(1), newDelimSt)
    }

    // else, normal mode
    else {
      st = newDelimSt
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

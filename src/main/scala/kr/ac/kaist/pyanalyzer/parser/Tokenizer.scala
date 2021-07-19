package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.collection.mutable.Stack
import kr.ac.kaist.pyanalyzer.util.Useful._
import org.apache.commons.text.StringEscapeUtils._

// TODO change to Parser[Token]
object Tokenizer extends Tokenizers {
  def tokenizeText(source: String): List[Token] =  {
    IndentParser().tokenizeSource(source)
  }
}

trait Tokenizers extends RegexParsers {
  val setlog = false
  def logg[T](p: Parser[T])(s: String) = if (setlog){ log(p)(s)} else { p }
  // line, comments, indents, whitespaces
  lazy val comment = "#[^\n]*".r
  // TODO implicit line joining
  lazy val whitespace = """[ \f\t]*""".r //TODO just use \s in needed
 
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
  lazy val shortSingleQuote = "'".r ~> "([^\n\\\\']|\\\\.)*".r <~ "'".r
  lazy val shortDoubleQuote = "\"".r ~> "([^\n\\\\\"]|\\\\.)*".r <~ "\"".r
  lazy val shortQuote = not("'''|\"\"\"".r) ~> (shortSingleQuote | shortDoubleQuote)
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
  lazy val tokens: Parser[List[Token]] = logg(rep(token))("tokens")

  // line end : take account of comment, explicit line joining, and starting of longstring
  lazy val longSingleStart = logg("'''.*".r)("lss")
  lazy val longDoubleStart = logg("\"\"\"[\\s\\S]*".r)("lds")  
  lazy val lineEnd: Parser[List[Token]] = logg((
    comment ^^ { case s => List(CommentToken(s), NewlineToken()) } |
    longSingleStart ^^ { case s => List(LongStrToken("'''", s)) } |
    longDoubleStart ^^ { case s => List(LongStrToken(""""""""", s)) } |
    """\\""".r ^^ { case _ => List(ExpJoinToken) }
  ))("lineend")
  lazy val newline = logg("\n".r)("newline")

  // line tokenizer considering comment and newline
  // input string must end with \n
  lazy val line: Parser[List[Token]] = tokens ~ opt(lineEnd) ^^ {
    case tl ~ None => tl :+ NewlineToken()
    case tl ~ Some(el) => tl ++ el
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
case object Break extends Exception
case object Continue extends Exception
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
    // assert(parsed.length > 0)("line parse result must be nonempty")
    // assert(parsed.last match {
    //  case NewlineToken(_) => true
    //  case _ => false
    // })("last token must be Newline")

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

  // tokenizes the whole source code, considering each lines
  def tokenizeSource(text: String): List[Token] = {
    val lines = text.split("\n") //.map(s => s + "\n") 
    var indents: List[Int] = List(0)
    var needCont: Boolean = false
    var contStr: String = ""
    var contStrQuote: String = ""
    var contLine: String = ""
    var continued: Boolean = false
    var parenLev: Int = 0

    var result: List[Token] = Nil

    // line will be nonempty. (at least contain \n)
    try {
      for (line <- lines) {
      //////////////////////////////////////////
        try {
          //////////////////////////////////////////
          // println(s"current tokens: $result")
          // println(s"line trying:\n------\n$line\n------\n")
          var curLine = line
          // continued string case
          if (contStr.nonEmpty) {
            // try find the quote, `'''` or `"""`
            contStrQuote.r.findFirstMatchIn(curLine) match {
              // match found
              case Some(m) =>
                result = result :+ StrToken(contStr + curLine.substring(0, m.start))
                curLine = curLine.substring(m.end)
                result = result :+ NewlineToken()
                contStr = ""
                needCont = false
                contLine = ""
                throw Continue
              // match not found
              case None =>
                contStr = contStr + curLine
                contLine = contLine + curLine
                throw Continue
            }
          }
          // new statement case
          else if (parenLev == 0 && !continued) { 
            // process indentation
            val trimmed = leftTrim(curLine) 
            val indentLen = curLine.length - trimmed.length
            // skip comments or blank lines
            if (trimmed.startsWith("#")) {
              result = result ++ List(CommentToken(trimmed), NewlineToken())
              throw Continue
            }
            if (trimmed.startsWith("\n")) {
              result = result :+ NewlineToken()
              throw Continue
            }
            // skip blank line?
            if (trimmed.isEmpty) {
              throw Continue
            }
            // after this, plain parsing routine will process remining line
            
            // normal case -> process indentation
            // indent case
            if (indentLen > indents.last) {
              result = result :+ IndentToken
              indents = indents :+ indentLen
            }
            // dedent case
            while (indentLen < indents.last) {
              if (!indents.contains(indentLen)) throw IndentError(indents.last, indentLen)
              indents = indents.init
              result = result :+ DedentToken
            } 
            //println(indents)
          }
          // continued statement case
          else {
            continued = false
            // just process the line tokens and add to the last
          }
          // actually parsing the line content
          // this should contain newline token if necessary
          val parsed: List[Token] = Tokenizer.parseText(curLine)
          // scan thru parsed to update parenLev
          parsed.foreach(t => t match {
            case DelimToken("(") => parenLev += 1
            case DelimToken("[") => parenLev += 1
            case DelimToken("{") => parenLev += 1
            case DelimToken(")") => parenLev -= 1
            case DelimToken("]") => parenLev -= 1
            case DelimToken("}") => parenLev -= 1
            case _  => 
          })
          // case analysis on last token
          parsed.last match {
            // ended with newline
            case NewlineToken(_) => {
              // continued case: remove newline
              if (parenLev > 0) { 
                // if comment token exist, 
                if (parsed.length >= 2) parsed.init.last match {
                  case CommentToken(_) => result = result ++ parsed.init.init
                  case _ => result = result ++ parsed.init 
                }
                // just normal 
              } else { result = result ++ parsed }
            }
            // explicit join case
            case ExpJoinToken => {
              continued = true 
              result = result ++ parsed.init
            }
            // long string case: start contStr
            case LongStrToken(q, s) => {
              result = result ++ parsed.init
              contStrQuote = q
              contStr = s.replaceFirst(q, "")
              needCont = true
              contLine = curLine
            }
          }

         //////////////////////////////////////////
        } catch {
          case Continue =>
        }
      //////////////////////////////////////////
      }
    } catch {
      case Break =>
    }
    //////////////////////////////////////////
    // flush remaining dedents
    indents.init.foreach(t => { result = result :+ DedentToken })
    // add endmarker
    result :+ EndToken
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

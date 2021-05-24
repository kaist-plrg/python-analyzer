package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  def apply(tokens: List[Token]): List[Stmt] = ??? 
}
trait TokenListParsers extends Parsers {
  type Elem = Token
  case class TokenPosition(column: Int, line: Int, protected val lineContents: String) extends Position
  abstract class TokenReader extends Reader[Token] { outer =>
    val tokens: List[Token]
    val pos: TokenPosition

    def isNewline: Boolean = tokens.head match {
      case Newline => true
      case _ => false
    }

    def width: Int = first.toString.length

    def atEnd: Boolean = tokens.isEmpty
    def first: Token = tokens.head
    def rest: TokenReader = new TokenReader {
      val tokens = outer.tokens.tail
      val pos = if (outer.isNewline) {
        TokenPosition(outer.pos.line + 1, outer.pos.column + outer.width, first.toString)
      } else {
        TokenPosition(outer.pos.line, outer.pos.column + outer.width, first.toString)
      }
    }
  }
  object TokenReader {
    def apply(ts: List[Token]): TokenReader = new TokenReader {
      val tokens = ts
      val stringList = List("") // TODO
      val pos = TokenPosition(1, 1, stringList.head)
    }
  }

  private def firstMap[T](in: Input, f: Token => ParseResult[T]): ParseResult[T] = {
    if (in.atEnd) Failure("EOF", in)
    else f(in.first)
  }
  
  // TODO write good failure messages
  // identifiers
  lazy val id: Parser[String] = Parser(in => firstMap(in, _ match {
    case Id(name) => Success(name, in.rest)
    case t => Failure(s"", in)
  }))

  // literals
  lazy val stringLiteral: Parser[String] = Parser(in => firstMap(in, _ match {
    case StrLiteral(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val bytesLiteral: Parser[String] = Parser(in => firstMap(in, _ match {
    case BytesLiteral(b) => Success(b, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val intLiteral: Parser[Int] = Parser(in => firstMap(in, _ match {
    case IntLiteral(i) => Success(i, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val floatLiteral: Parser[Double] = Parser(in => firstMap(in, _ match {
    case FloatLiteral(f) => Success(f, in.rest)
    case t => Failure(s"", in) 
  }))

  lazy val imagLiteral: Parser[Double] = Parser(in => firstMap(in, _ match {
    case ImagLiteral(i) => Success(i, in.rest)
    case t => Failure(s"", in)
  }))

  // statements
  lazy val statements: Parser[List[Stmt]] = rep(statement)
  lazy val statement: Parser[Stmt] = compoundStmt | simpleStmt
  
  lazy val compoundStmt: Parser[Stmt] = ???
  lazy val simpleStmt: Parser[Stmt] = ???
}

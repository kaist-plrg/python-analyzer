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

  // keywords, op and delimiters
  lazy val keyword: Parser[String] = Parser(in => firstMap(in, _ match {
    case Keyword(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val op: Parser[String] = Parser(in => firstMap(in, _ match {
    case Op(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val delim: Parser[String] = Parser(in => firstMap(in, _ match {
    case Delim(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  // literal
  lazy val literal : Parser[Atom] = (stringLiteral | bytesLiteral | intLiteral | floatLiteral | imagLiteral) ^^ {
    case s => ???
  }
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

  implicit def text(s: String): Parser[String] = {
    Parser(in => {
      (keyword | op | delim)(in)
    })
  }

  // statements
  lazy val statements: Parser[List[Stmt]] = rep(statement)
  lazy val statement: Parser[Stmt] = compoundStmt | simpleStmt
  
  lazy val compoundStmt: Parser[Stmt] = ???
  lazy val simpleStmt: Parser[Stmt] = ???

  // expressions
  lazy val expression: Parser[Expr] = condExpr | lambdaExpr
  lazy val condExpr: Parser[Expr] = ???
  lazy val lambdaExpr: Parser[Expr] = ???
  
  lazy val exprList: Parser[List[Expr]] = expression ~ (op ~ expression).* ^^ {
    case e1 ~ l => ??? 
  }

  lazy val orTest: Parser[Expr] = andTest | orTest ~ keyword ~ andTest ^^ {
    case e1 ~ e2 => ??? 
  }
  lazy val andTest: Parser[Expr] = notTest | andTest ~ keyword ~ notTest ^^ {
    case e1 ~ "and" ~ e2 => ??? 
  }
  lazy val notTest: Parser[Expr] = comparison | keyword ~ notTest ^^ {
    case "not" ~ e => ??? 
  }

  lazy val comparison: Parser[Expr] = orExpr ~ rep(compOp ~ orExpr) ^^ {
    case e ~ l => ??? 
  }
  
  private val compOpList = List("<", ">", "==", ">=", "<=", "!=") //TODO how to add "is, not, in" ?
  lazy val compOp: Parser[Op] = op ^^ {
    case s if compOpList contains s => CompareOp(s)
  }

  lazy val andExpr: Parser[Expr] = shiftExpr | andExpr ~ op ~ shiftExpr ^^ {
    case e1 ~ "&" ~ e2 => ???
  }
  lazy val xorExpr: Parser[Expr] = andExpr | xorExpr ~ op ~ andExpr ^^ {
    case e1 ~ "^" ~ e2 => ???
  }
  lazy val orExpr: Parser[Expr] = xorExpr | orExpr ~ op ~ xorExpr ^^ {
    case e1 ~ "|" ~ e2 => ???
  }

  lazy val shiftExpr: Parser[Expr] = aExpr | shiftExpr ~ op ~ aExpr ^^ {
    case e1 ~ o ~ e2 if List("<<", ">>") contains o => ???  
  }

  lazy val aExpr: Parser[Expr] = mExpr | aExpr ~ op ~ mExpr ^^ {
    case e1 ~ o ~ e2 if List("+", "-") contains o => ???
  }
  lazy val mExpr: Parser[Expr] = uExpr | mExpr ~ op ~ uExpr ^^ {
    case e1 ~ o ~ e2 if List("*", "//", "/") contains o => ???
  } | mExpr ~ op ~ mExpr ^^ {
    case e1 ~ "@" ~ e2 => ???
  }
  
  lazy val uExpr: Parser[Expr] = power | op ~ uExpr ^^ {
    case o ~ uExpr if List("-", "+", "~") contains o => ???
  }

  lazy val power: Parser[Expr] = (awaitExpr | primary) ~ opt(op ~ uExpr) ^^ {
    case e1 ~ None => ???
    case e1 ~ Some("**" ~ e2) => ???
  }

  lazy val awaitExpr: Parser[Expr] = keyword ~ primary ^^ {
    case "await" ~ primary => ???  
  }

  lazy val primary: Parser[Expr] = atom | attrRef | subscription | slicing | call
  lazy val atom: Parser[Atom] = id ^^ { AId(_) } | literal | enclosure
  lazy val enclosure: Parser[Atom] = ???
  
  lazy val attrRef: Parser[Expr] = primary ~ delim ~ id ^^ {
    case e1 ~ "." ~ e2 => ???
  }
  lazy val subscription: Parser[Expr] = primary ~ delim ~ exprList ~ delim ^^ {
    case e1 ~ "(" ~ l ~ ")" => ???
  }

  lazy val slicing: Parser[Expr] = primary ~ delim ~ sliceList ~ delim ^^ {
    case e1 ~ "[" ~ l ~ "]" => ???
  }
  lazy val sliceList: Parser[List[Expr]] = ???

  lazy val call: Parser[Expr] = primary ~ delim ~ argList ~ delim ^^ {
    case e1 ~ "(" ~ l ~ ")" => ??? 
  }
  lazy val argList: Parser[List[Expr]] = ???
}

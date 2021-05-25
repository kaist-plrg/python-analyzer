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
  lazy val id: Parser[AId] = Parser(in => firstMap(in, _ match {
    case Id(name) => Success(AId(name), in.rest)
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

  lazy val namedLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case Keyword(s) if s == ATrue => Success(ATrue, in.rest)
    case Keyword(s) if s == AFalse => Success(AFalse, in.rest)
    case Keyword(s) if s == ANone => Success(ANone, in.rest)
    case _ => Failure(s"", in)
  }))
  
  lazy val stringLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case StrLiteral(s) => Success(AStringLiteral(s), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val bytesLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case BytesLiteral(b) => Success(ABytesLiteral(b), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val intLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case IntLiteral(i) => Success(AIntLiteral(i.toInt), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val floatLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case FloatLiteral(f) => Success(AFloatLiteral(f.toDouble), in.rest)
    case t => Failure(s"", in) 
  }))

  lazy val imagLiteral: Parser[Atom] = Parser(in => firstMap(in, _ match {
    case ImagLiteral(i) => Success(AImagLiteral(i.toDouble), in.rest)
    case t => Failure(s"", in)
  }))

  implicit def text(s: String): Parser[String] = Parser(op | delim | keyword)

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

  lazy val ListOfBinaryExpr = (op: Op, expr: Expr, le: List[Expr]) =>
    le.foldLeft(expr)((tempRes, e) => BinaryExpr(op, tempRes, e))
  lazy val orExpr: Parser[Expr] = andExpr ~ rep("or" ~> andExpr) ^^ {
    case e ~ le => ListOfBinaryExpr(COr, e, le)
  }
  lazy val andExpr: Parser[Expr] = notExpr ~ rep("and" ~> notExpr) ^^ {
    case e ~ le => ListOfBinaryExpr(CAnd, e, le)
  }
  lazy val notExpr: Parser[Expr] = "not" ~> notExpr ^^ {
    case e => UnaryExpr(CNot, e)
  } | compExpr

  lazy val cop: Parser[COp] = (
    "==" ^^^ CEq |
    "!=" ^^^ CNeq |
    "<=" ^^^ CLte |
    "<" ^^^ CLt |
    ">=" ^^^ CGte |
    ">" ^^^ CGt |
    "not in" ^^^ CNotIn |
    "in" ^^^ CIn |
    "is not" ^^^ CIsNot |
    "is" ^^^ CIs
  )

  lazy val compExpr: Parser[Expr] = bOrExpr ~ rep(cop ~ bOrExpr) ^^ {
    case be ~ l if l.isEmpty => be
    case be ~ (h :: t) =>
      t.foldLeft((BinaryExpr(h._1, be, h._2), h._2)) ((tup, e) => {
        val (tempRes, lhs) = tup
        val ~(op, rhs) = e
        (BinaryExpr(CAnd, tempRes, BinaryExpr(op, lhs, rhs)), rhs)
      }
    )._1
  }

  lazy val bOrExpr: Parser[Expr] = opt(bOrExpr <~ "|") ~ bXorExpr ^^ {
    case None ~ e => e
    case Some(e1) ~ e2 => BinaryExpr(OBXor, e1, e2)
  }
  lazy val bAndExpr: Parser[Expr] = opt(bAndExpr <~ "&") ~ shiftExpr ^^ {
    case None ~ e => e
    case Some(e1) ~ e2 => BinaryExpr(OBAnd, e1, e2)
  }
  lazy val bXorExpr: Parser[Expr] = opt(bXorExpr <~ "^") ~ bAndExpr ^^ {
    case None ~ e => e
    case Some(e1) ~ e2 => BinaryExpr(OBXor, e1, e2)
  }

  lazy val bop: Parser[BOp] = (
    "<<" ^^^ OLShift |
    ">>" ^^^ ORShift |
    "+" ^^^ OPlus |
    "-" ^^^ OSub |
    "*" ^^^ OMul |
    "/" ^^^ ODiv |
    "//" ^^^ OIDiv |
    "%" ^^^ OMod |
    "@" ^^^ OMMul
  )

  lazy val uop: Parser[UOp] = (
    "+" ^^^ OUPlus |
    "-" ^^^ OUMinus |
    "~" ^^^ OUInv
  )

  lazy val shiftExpr: Parser[Expr] = shiftExpr ~ bop ~ aExpr ^^ {
    case e1 ~ b ~ e2 => BinaryExpr(b, e1, e2)
  } | aExpr

  lazy val aExpr: Parser[Expr] = aExpr ~ bop ~ mExpr ^^ {
    case e1 ~ b ~ e2 => BinaryExpr(b, e1, e2)
  } | mExpr
  lazy val mExpr: Parser[Expr] = mExpr ~ bop ~ uExpr ^^ {
    case e1 ~ b ~ e2 => BinaryExpr(b, e1, e2)
  } | uExpr
  
  lazy val uExpr: Parser[Expr] = uop ~ uExpr ^^ {
    case u ~ e => UnaryExpr(u, e)
  } | power

  lazy val power: Parser[Expr] = awaitExpr ~ opt("**" ~> uExpr) ^^ {
    case e1 ~ None => e1
    case e1 ~ Some(e2) => BinaryExpr(OPow, e1, e2)
  }

  lazy val awaitExpr: Parser[Expr] = ("await" ~> primary) | primary

  lazy val primary: Parser[Expr] = invalid_primary | attrRef | primary_gen |
    call | slicing | atom
  lazy val atom: Parser[Atom] = id | namedLiteral | stringLiteral |
      bytesLiteral | intLiteral | floatLiteral | imagLiteral | enclosure
  lazy val enclosure: Parser[Atom] = ???

  lazy val invalid_primary: Parser[Expr] = ???

  lazy val primary_gen: Parser[Expr] = primary ~ genExpr ^^ {
    case e1 ~ e2 => ???
  }

  lazy val genExpr: Parser[Expr] = ???
  
  lazy val attrRef: Parser[Expr] = primary ~ ("." ~> id) ^^ {
    case e1 ~ x => ???
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

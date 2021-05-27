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

    // expressions
  lazy val listOfExpr = (p: Parser[Expr]) => p ~ rep("," ~> p) <~ opt(",") ^^ {
    case e ~ le => e :: le
  }
  // TODO: conver it to tuple
  lazy val starExprList: Parser[List[Expr]] = listOfExpr(starExpr)
  lazy val starExpr: Parser[Expr] = "*" ~> bOrExpr ^^ StarExpr | expr

  // TODO: conver it to tuple
  lazy val starNamedExprList: Parser[List[Expr]] = listOfExpr(starNamedExpr)
  lazy val starNamedExpr: Parser[Expr] = "*" ~> bOrExpr ^^ StarExpr | namedExpr

  // TODO: handle ~ in the spec
  lazy val assignExpr: Parser[Expr] = id ~ (":=" ~> expr) ^^ {
    case x ~ e => AssignExpr(x, e)
  }

  lazy val namedExpr: Parser[Expr] = assignExpr | exprNeg

  lazy val expr: Parser[Expr] = condExpr | lambdaExpr
  lazy val exprNeg: Parser[Expr] = expr - ":="
  lazy val condExpr: Parser[Expr] = orExpr ~ opt("if" ~> orExpr ~ ("else" ~> expr)) ^^ {
    case oe1 ~ Some(oe2 ~ e) => CondExpr(oe2, oe1, e)
    case oe ~ None => oe

  }
  lazy val lambdaExpr: Parser[Expr] = ("lambda" ~> paramList <~ ":") ~ expr ^^ {
    case pl ~ e => LambdaExpr(pl, e)
  }

  // TODO: conver it to tuple
  lazy val exprList: Parser[List[Expr]] = listOfExpr(expr)
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
    case be ~ Nil => be
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

  lazy val primary: Parser[Primary] = invalid_primary | attrRef | primary_gen | call | slicing | atom
  lazy val atom: Parser[Atom] = id | namedLiteral | stringLiteral |
      bytesLiteral | intLiteral | floatLiteral | imagLiteral | enclosure
  lazy val enclosure: Parser[Atom] = ???

  lazy val invalid_primary: Parser[Primary] = ???

  lazy val primary_gen: Parser[Primary] = primary ~ genExpr ^^ {
    case e1 ~ e2 => ???
  }

  lazy val genExpr: Parser[Expr] =  "(" ~> (assignExpr | exprNeg) ~
    forIfClause ^^ {
    case e ~ lc => ???
  }

  lazy val forIfClause: Parser[List[Expr]] = ???
  
  lazy val attrRef: Parser[Primary] = primary ~ ("." ~> id) ^^ {
    case p ~ i => EAttrRef(p, i) 
  }
  lazy val subscription: Parser[Primary] = primary ~ ("[" ~> exprList <~ "]") ^^ {
    case p ~ l => ESubscript(p, l)
  }

  lazy val slicing: Parser[Primary] = primary ~ ("[" ~> sliceList <~ "]") ^^ {
    case p ~ l => Slicing(p, l)  
  }
 
  lazy val sliceList: Parser[List[Expr]] = sliceItem ~ rep("," ~> sliceItem) <~ opt(",") ^^ {
    case fst ~ rst => fst +: rst
  }
  lazy val sliceItem: Parser[Expr] = expr | properSlice
  lazy val properSlice: Parser[Expr] = "[" ~> expr ~ (":" ~> expr ~ opt(":" ~> expr)) ^^ {
    case lb ~ (ub ~ None) => Slice(lb, ub, AIntLiteral(1)) 
    case lb ~ (ub ~ Some(s)) => Slice(lb, ub, s)
  }

  // TODO should add comprehension
  lazy val call: Parser[Primary] = primary ~ ("(" ~> argList <~ ")") ^^ {
    case p ~ pair => Call(p, pair._1, pair._2, EEmpty)
  }

  // Call argument list - look at Expr.scala, case class Call
  // arguments are largely divided into 
  lazy val argList: Parser[(List[Expr], Map[AId, Expr], Option[Expr])] = posArgs ~ opt("," ~> starredAndKeywords) ~ opt("," ~> keywordArgs) ^^ {
    case pargs ~ a2 ~ Some((kwargs, e)) => (pargs, kwargs, e)
    case pargs ~ a2 ~ None => (pargs, Map.empty, None)
  }
  lazy val posArgs: Parser[List[Expr]] = repsep(posItem, ",")
  lazy val posItem: Parser[Expr] = assignExpr | "*" ~> expr 

  // TODO should add keywordargs
  lazy val starredAndKeywords: Parser[List[Expr]] = repsep(("*" ~> expr), ",")

  // TODO really messy here, should refactor
  val kargsFolder: ( (Map[AId, Expr], Option[Expr]), Any) => (Map[AId, Expr], Option[Expr]) = (sum, res) => (sum, res) match {
    case ( (map, exprOpt), (i: AId, e: Expr) ) => (map + (i -> e), exprOpt) // keywordItem case
    case ( (map, exprOpt), (e: Expr) ) => (map, Some(e)) // "**" ~> expr case
    case ( (map, exprOpt), _ ) => ??? // impossible
  }
  lazy val keywordArgs: Parser[(Map[AId, Expr], Option[Expr])] = repsep((keywordItem | "**" ~> expr), ",") ^^ (l => {
    val initMap: Map[AId, Expr] = Map.empty
    val initOpt: Option[Expr] = None
    l.foldLeft( (initMap, initOpt) )(kargsFolder)
  })

  // TODO model keyword_item properly
  lazy val keywordItem: Parser[(AId, Expr)] = id ~ ("=" ~> expr) ^^ {
    case i ~ e => (i, e)
  }

  // Statements
  lazy val statements: Parser[List[Stmt]] = rep(statement)
  lazy val statement: Parser[Stmt] = compoundStmt | simpleStmt
  
  lazy val compoundStmt: Parser[Stmt] = ???
  lazy val simpleStmt: Parser[Stmt] = ???
  lazy val suite: Parser[Stmt] = funcdef // TODO add others

  // TODO complete this
  lazy val funcdef: Parser[Stmt] =
    opt(decorators) ~ ("def" ~> id ~ ("(" ~> opt(paramList) <~ ")")) ~ (":" ~> suite) ^^ {
      // TODO add yes decorator case
      case None ~ (i ~ None) ~ s => ???
      case None ~ (i ~ Some(pl)) ~ s => ???
    }
  lazy val decorators: Parser[List[Expr]] = ??? 
  lazy val paramList: Parser[List[Param]] =
    defparam ~ ("," ~> defparam).* ~ (("," ~ "/") ~> opt("," ~> opt(paramListNoPosonly))) ^^ {
      // no posonly
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes posonly
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListNoPosonly

  lazy val paramListNoPosonly: Parser[List[Param]] =
    defparam ~ ("," ~> defparam).* ~ opt("," ~> opt(paramListStarargs)) ^^ {
      // no starparams
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes starparams
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListStarargs

  // parser for parameters appear after star or double-star
  lazy val paramListStarargs: Parser[List[Param]] =
    oneStarParam ~ ("," ~> defparam).* ~ opt("," ~> opt(doubleStarParam)) ^^ {
      // no kwargs
      case o ~ pl ~ None => o.toList ++ pl 
      case o ~ pl ~ Some(None) => o.toList ++ pl 
      // yes kwargs
      case o ~ pl ~ Some(Some(p)) => o.toList ++ pl ++ List(p) 
    } ^^ { pl => pl.map({
      case p: NormalParam => p.copy(keyOnly=true)
      case p: ArbPosParam => p
      case p: ArbKeyParam => p
    })} | 
    doubleStarParam <~ opt(",") ^^ { List(_) }

  // parser for arbitrary positional parameter
  lazy val oneStarParam: Parser[Option[Param]] = ("*" ~> opt(param)) ^^ {
    case Some(i) => Some(ArbPosParam(i))
    case None => None
  }
  // parser for arbitrary keyword parameter
  lazy val doubleStarParam: Parser[Param] = "**" ~> param <~ opt(",") ^^ {
    case i => ArbKeyParam(i) 
  }
  // parser for normal parameter
  lazy val defparam: Parser[Param] = param ~ ("=" ~> opt(expr)) ^^ {
    case i ~ oe => NormalParam(i, 0, oe, false)
  }
  // parser for parameter id
  lazy val param: Parser[AId] = id // TODO add optional type expr `: expr`
}

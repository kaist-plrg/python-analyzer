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

  lazy val number: Parser[Atom] = intLiteral | floatLiteral | imagLiteral
  ///////////////////////////////////////////////
  // expressions
  lazy val starExpressions: Parser[List[Expr]] = repsep(starExpr, ",") <~ opt(",") ^^ { ??? }
  lazy val starExpr: Parser[Expr] =
    "*" ~> bitOr | namedExpr 
  lazy val starNamedExpressions: Parser[List[Expr]] = repsep(starNamedExpr, ",") <~ opt(",") ^^ { ??? } 
  lazy val starNamedExpr: Parser[Expr] =
    "*" ~> bitOr | namedExpr
  lazy val assignExpr: Parser[Expr] = id ~ (":=" ~> commit(expression)) ^^ { ??? }
  lazy val namedExpr: Parser[Expr] =
    assignExpr | expression - ":="
  lazy val expressions: Parser[List[Expr]] = repsep(expression, ",") <~ opt(",") ^^ {
    ???
  }
  lazy val expression: Parser[Expr] =
    lambdef |
    disjunction |
    disjunction ~ ("if" ~> disjunction ~ ("else" ~> expression)) ^^ {
      ???
    }
  // lambda expressions
  lazy val lambdef: Parser[Expr] = ("lambda" ~> opt(lambdaParams <~ ":")) ~ expression ^^ {
    case Some(pl) ~ e => LambdaExpr(pl, e)
  }
  // TODO replace rep to x.+
  lazy val lambdaParams: Parser[List[Param]] =
    lambdaSlashNoDefault ~ lambdaParamNoDefault.* ~ lambdaParamWithDefault.* ~ opt(lambdaStarEtc) ^^ {
      ???
    } |
    lambdaSlashWithDefault ~ lambdaParamWithDefault.* ~ opt(lambdaStarEtc) ^^ {
      ???
    } |
    rep(lambdaParamNoDefault) ~ lambdaParamWithDefault.* ~ opt(lambdaStarEtc) ^^ {
      ???
    } |
    rep(lambdaParamWithDefault) ~ opt(lambdaStarEtc) ^^ {
      ???
    } |
    lambdaStarEtc ^^ {
      ???
    }
  lazy val lambdaSlashNoDefault: Parser[List[Param]] =
    rep(lambdaParamNoDefault) <~ ("/" ~ ",") ^^ {
      ???
    } |
    rep(lambdaParamNoDefault) <~ ("/" + ":") ^^ {
      ???
    }
  lazy val lambdaSlashWithDefault: Parser[List[Param]] =
    lambdaParamNoDefault.* ~ rep(lambdaParamWithDefault) <~ ("/" ~ ",") ^^ {
      ???
    } |
    lambdaParamNoDefault.* ~ rep(lambdaParamWithDefault) <~ ("/" + ":") ^^ {
      ???
    }
  lazy val lambdaStarEtc: Parser[List[Param]] = 
    "*" ~> lambdaParamNoDefault ~ lambdaParamMaybeDefault.* ~ opt(lambdaKwds) ^^ {
      ???
    } |
    ("*" ~ ",") ~> rep(lambdaParamMaybeDefault) ~ opt(lambdaKwds) ^^ {
      ???
    } |
    lambdaKwds
  lazy val lambdaKwds: Parser[List[Param]] = "**" ~> lambdaParamNoDefault ^^ {
    ???
  }
  lazy val lambdaParamNoDefault: Parser[Param] =
    (lambdaParam <~ "," | lambdaParam ~ guard(":")) ^^ {
        ???
    }
  lazy val lambdaParamWithDefault: Parser[Param] =
  (lambdaParam ~ default <~ "," | lambdaParam ~ default ~ guard(":")) ^^ {
    ???
  }
  lazy val lambdaParamMaybeDefault: Parser[Param] =
  (lambdaParam ~ opt(default) <~ "," | lambdaParam ~ opt(default) ~ guard(":")) ^^ {
    ???
  } 
  lazy val lambdaParam: Parser[AId] = id

  // Expressions : production rules
  lazy val disjunction: Parser[Expr] =
    conjunction ~ rep("or" ~> conjunction) ^^ {
      ???
    } | conjunction
  lazy val conjunction: Parser[Expr] =
    inversion ~ rep("and" ~> inversion) ^^ {
      ???
    } | inversion
  lazy val inversion: Parser[Expr] = "not" ~> inversion | comparison
  lazy val comparison: Parser[Expr] =
    bitOr ~ rep(compareOpBitOrPair) ^^ {
      ???
    } | bitOr
  lazy val compareOpBitOrPair: Parser[Expr] =
    eqBitOr | noteqBitOr | lteBitOr | ltBitOr | gteBitOr | gtBitOr | notinBitOr | inBitOr | isnotBitOr | isBitOr
  lazy val eqBitOr: Parser[Expr] = "==" ~> bitOr 
  lazy val noteqBitOr: Parser[Expr] = "!=" ~> bitOr
  lazy val lteBitOr: Parser[Expr] = "<=" ~> bitOr
  lazy val ltBitOr: Parser[Expr] = "<" ~> bitOr
  lazy val gteBitOr: Parser[Expr] = ">=" ~> bitOr
  lazy val gtBitOr: Parser[Expr] = ">" ~> bitOr
  lazy val notinBitOr: Parser[Expr] = ("not" ~ "in") ~> bitOr
  lazy val inBitOr: Parser[Expr] = "in" ~> bitOr
  lazy val isnotBitOr: Parser[Expr] = ("is" ~ "not") ~> bitOr
  lazy val isBitOr: Parser[Expr] = "is" ~> bitOr
  lazy val bitOr: Parser[Expr] = bitOr ~ ("|" ~> bitXor) ^^ {
    ???
  } | bitXor
  lazy val bitXor: Parser[Expr] = bitXor ~ ("^" ~> bitAnd) ^^ {
    ???
  } | bitAnd
  lazy val bitAnd: Parser[Expr] = bitAnd ~ ("&" ~> shiftExpr) ^^ {
    ???
  } | shiftExpr
  lazy val shiftExpr: Parser[Expr] = 
    shiftExpr ~ ("<<" ~> sum) ^^ {
      ???
    } |
    shiftExpr ~ (">>" ~> sum) ^^ {
      ???
    } | sum
  lazy val sum: Parser[Expr] = 
    sum ~ ("+" ~> term) ^^ {
      ???
    } |
    sum ~ ("-" ~> term) ^^ {
      ???
    } | term
  lazy val term: Parser[Expr] =
    term ~ ("*" ~> factor) ^^ {
      ???
    } |
    term ~ ("/" ~> factor) ^^ {
      ???
    } |
    term ~ ("//" ~> factor) ^^ {
      ???
    } |
    term ~ ("%" ~> factor) ^^ {
      ???
    } |
    term ~ ("@" ~> factor) ^^ {
      ???
    } | factor
  lazy val factor: Parser[Expr] =
    "+" ~> factor | "-" ~> factor | "~" ~> factor | power 
  lazy val power: Parser[Expr] =
    awaitPrimary ~ ("**" ~> factor) ^^ {
      ???
    } | awaitPrimary
  lazy val awaitPrimary: Parser[Expr] =
    "await" ~> primary | primary
  lazy val primary: Parser[Expr] =
    //invalidPrimary |
    primary ~ ("." ~> id) ^^ { ??? } |
    primary ~ genexp ^^ {???} |
    primary ~ ("(" ~> opt(args) <~ ")") ^^ { ??? } |
    primary ~ ("[" ~> slices <~ "]") ^^ { ??? } |
    atom
  lazy val slices: Parser[List[Expr]] =
    slice ~ not(",") ^^ { ??? } |
    repsep(slice, ",") ~ opt(",") ^^ { ??? }
  lazy val slice: Parser[Expr] =
    opt(expression) ~ (":" ~> opt(expression)) ~ opt(":" ~> opt(expression)) ^^ {
      ???
    } | namedExpr
  
  // atoms : literal-like production
  lazy val atom: Parser[Expr] =
    id ^^ { ??? } |
    "True" ^^ { ??? } |
    "False" ^^ { ??? } |
    strings |
    number |
    (tuple | group | genexp) |
    (list | listcomp) |
    (dict | set | dictcomp | setcomp)

  // TODO make primitive parser for these
  lazy val strings: Parser[Expr] = ???
  lazy val list: Parser[Expr] = "[" ~> opt(starNamedExpr) <~ "]" ^^ { ??? }
  lazy val listcomp: Parser[Expr] = "[" ~> (namedExpr ~ forIfClauses) <~ "]" ^^ { ??? }  
  lazy val tuple: Parser[Expr] = "(" ~> opt(starNamedExpr ~ ("," ~> opt(starNamedExpr))) <~ ")" ^^ { ??? }
  lazy val group: Parser[Expr] = "(" ~> (yieldExpr | namedExpr) <~ ")" ^^ { ??? }
  lazy val genexp: Parser[Expr] = "(" ~> (assignExpr | expression - ":=") ~ forIfClauses <~ ")" ^^ { ??? }
  lazy val set: Parser[Expr] = "{" ~> starNamedExpr <~ "}" ^^ { ??? }
  lazy val setcomp: Parser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ { ??? } 
  lazy val dict: Parser[Expr] = 
    "{" ~> opt(doubleStarredKvpairs) <~ "}" ^^ { ??? }
    // | "{" ~> invalidDoudlbeStarredKvpairs <~ "}" ^^ { ??? }
  lazy val dictcomp: Parser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ { ??? }
  lazy val doubleStarredKvpairs: Parser[List[Expr]] = repsep(doubleStarredKvpair, ",") ~ opt(",") ^^ { ??? }
  lazy val doubleStarredKvpair: Parser[Expr] = "**" ~> bitOr | kvPair 
  lazy val kvPair: Parser[Expr] = expression ~ (":" ~> expression) ^^ { ??? }
  // Comprehensions
  // 
  lazy val forIfClauses: Parser[List[Expr]] = rep(forIfClause) 
  // comp_for
  lazy val forIfClause: Parser[Expr] =
    (opt("async") ~ "for") ~> starTargets ~ ("in" ~> disjunction ~ ("if" ~> disjunction).*) ^^ {
      ???
    }
  lazy val yieldExpr: Parser[Expr] =
    ("yield" ~ "from") ~> expression ^^ { ??? } |
    "yeild" ~> opt(starExpressions) ^^ { ??? }
  
  // arguments
  lazy val arguments: Parser[List[Expr]] = args <~ (opt(",") - ")") ^^ {
    ???
  }
  lazy val args: Parser[List[Param]] = 
    repsep(starredExpr | (assignExpr | expression - ":=") - "=", ",") <~ opt(",") ^^ {
      ???
    } | kwargs
  lazy val kwargs: Parser[List[Param]] =
    repsep(kwargOrStarred, ",") <~ ("," ~ repsep(kwargOrDoubleStarred, ",")) ^^ {
      ???
    } |
    repsep(kwargOrStarred, ",") ^^ {
      ???
    } |
    repsep(kwargOrDoubleStarred, ",") ^^ {
      ???
    }
  lazy val starredExpr: Parser[Expr] = "*" ~> expression
  lazy val kwargOrStarred: Parser[Expr] =
    id ~ ("=" ~> expression) ^^ { ??? } | starredExpr
  lazy val kwargOrDoubleStarred: Parser[Expr] = 
    id ~ ("=" ~> expression) ^^ { ???} | "**" ~> expression

  // targets
  // TODO complete this (last part of full grammar)
  lazy val starTargets: Parser[List[Expr]] =  
    starTarget - "," ^^ { ??? } |
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTargetsListSeq: Parser[List[Expr]] =
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTargetTupleSeq: Parser[List[Expr]] =
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTarget: Parser[Expr] = ???
  // ...
  lazy val targets: Parser[List[Expr]] =
    repsep(target, ",") <~ opt(",") ^^ { ??? } 
  lazy val target: Parser[Expr] = ???

  ////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
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
  lazy val defparam: Parser[Param] = param ~ ("=" ~> opt(expression)) ^^ {
    case i ~ oe => NormalParam(i, 0, oe, false)
  }
  // parser for parameter id
  lazy val param: Parser[AId] = id // TODO add optional type expr `: expr`
  lazy val default: Parser[Expr] = "=" ~> expression ^^ { ??? }
}

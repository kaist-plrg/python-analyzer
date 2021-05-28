package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  def apply(tokens: List[Token]): List[Stmt] = ??? 
}
trait TokenListParsers extends Parsers {
  ///////////////////////////////////////////////////////////////////
  // Basic Parsers definition, token reader
  ///////////////////////////////////////////////////////////////////
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

  //////////////////////////////////////////////////////////////////
  // Parsing rule definitions
  // Python language specification - 10. Full grammar specification
  //////////////////////////////////////////////////////////////////

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
    case Keyword(s) if s == ABool(true) => Success(ABool(true), in.rest)
    case Keyword(s) if s == ABool(false) => Success(ABool(false), in.rest)
    case Keyword(s) if s == ANone => Success(ANone, in.rest)
    case _ => Failure(s"", in)
  }))
  
  // literals, number, name(id)
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

  lazy val number: Parser[Atom] = intLiteral | floatLiteral | imagLiteral

  implicit def text(s: String): Parser[String] = Parser(op | delim | keyword)

  ///////////////////////////////////////////////
  // expressions
  ///////////////////////////////////////////////
  
  lazy val starExprs: Parser[List[Expr]] = repsep(starExpr, ",") <~ opt(",") ^^ { ??? }
  lazy val starExpr: Parser[Expr] =
    "*" ~> bitOr | namedExpr 
  lazy val starNamedExprs: Parser[List[Expr]] = repsep(starNamedExpr, ",") <~ opt(",") ^^ { ??? } 
  lazy val starNamedExpr: Parser[Expr] =
    "*" ~> bitOr | namedExpr
  lazy val assignExpr: Parser[Expr] = id ~ (":=" ~> commit(expression)) ^^ { ??? }
  lazy val namedExpr: Parser[Expr] =
    assignExpr | expression - ":="
  lazy val expressions: Parser[List[Expr]] = repsep(expression, ",") <~ opt(",") 
  lazy val expression: Parser[Expr] =
    lambdef |
    disjunction |
    disjunction ~ ("if" ~> disjunction ~ ("else" ~> expression)) ^^ {
      case ie ~ (te ~ ee) => CondExpr(ie, te, ee)
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
    conjunction ~ rep1("or" ~> conjunction) ^^ {
      case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LOr, sum, elem) )
    } | conjunction
  lazy val conjunction: Parser[Expr] =
    inversion ~ rep1("and" ~> inversion) ^^ {
      case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LAnd, sum, elem) )
    } | inversion
  lazy val inversion: Parser[Expr] =
    "not" ~> inversion ^^ {
      case e => UnaryExpr(LNot, e)
    } | comparison
  lazy val comparison: Parser[Expr] =
    bitOr ~ rep1(compareOpBitOrPair) ^^ {
      case e ~ el => el.foldLeft(e)( (sum, elem) => elem match {
        case (op, e) => BinaryExpr(op, sum, e)
      })
    } | bitOr
  lazy val compareOpBitOrPair: Parser[(Op, Expr)] =
    eqBitOr | noteqBitOr | lteBitOr | ltBitOr | gteBitOr | gtBitOr | notinBitOr | inBitOr | isnotBitOr | isBitOr
  lazy val eqBitOr: Parser[(Op, Expr)] = "==" ~> bitOr ^^ { (CEq, _) }  
  lazy val noteqBitOr: Parser[(Op, Expr)] = "!=" ~> bitOr ^^ { (CNeq, _) }
  lazy val lteBitOr: Parser[(Op, Expr)] = "<=" ~> bitOr ^^ { (CLte, _) }
  lazy val ltBitOr: Parser[(Op, Expr)] = "<" ~> bitOr ^^ { (CLt, _) }
  lazy val gteBitOr: Parser[(Op, Expr)] = ">=" ~> bitOr ^^ { (CGte, _) }
  lazy val gtBitOr: Parser[(Op, Expr)] = ">" ~> bitOr ^^ { (CGt, _) }
  lazy val notinBitOr: Parser[(Op, Expr)] = ("not" ~ "in") ~> bitOr ^^ { (CNotIn, _) }
  lazy val inBitOr: Parser[(Op, Expr)] = "in" ~> bitOr ^^ { (CIn, _) }
  lazy val isnotBitOr: Parser[(Op, Expr)] = ("is" ~ "not") ~> bitOr ^^ { (CIsNot, _) }
  lazy val isBitOr: Parser[(Op, Expr)] = "is" ~> bitOr ^^ { (CIs, _) }

  lazy val bitOr: Parser[Expr] = bitOr ~ ("|" ~> bitXor) ^^ {
    case e1 ~ e2 => BinaryExpr(OBOr, e1, e2)
  } | bitXor
  lazy val bitXor: Parser[Expr] = bitXor ~ ("^" ~> bitAnd) ^^ {
    case e1 ~ e2 => BinaryExpr(OBXor, e1, e2) 
  } | bitAnd
  lazy val bitAnd: Parser[Expr] = bitAnd ~ ("&" ~> shiftExpr) ^^ {
    case e1 ~ e2 => BinaryExpr(OBAnd, e1, e2)
  } | shiftExpr
  lazy val shiftExpr: Parser[Expr] = 
    shiftExpr ~ ("<<" ~> sum) ^^ {
      case e1 ~ e2 => BinaryExpr(OLShift, e1, e2)
    } |
    shiftExpr ~ (">>" ~> sum) ^^ {
      case e1 ~ e2 => BinaryExpr(ORShift, e1, e2)
    } | sum

  lazy val sum: Parser[Expr] = 
    sum ~ ("+" ~> term) ^^ {
      case e1 ~ e2 => BinaryExpr(OAdd, e1, e2)
    } |
    sum ~ ("-" ~> term) ^^ {
      case e1 ~ e2 => BinaryExpr(OSub, e1, e2)
    } | term
  lazy val term: Parser[Expr] =
    term ~ ("*" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OMul, e1, e2) 
    } |
    term ~ ("/" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(ODiv, e1, e2)
    } |
    term ~ ("//" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OIDiv, e1, e2)
    } |
    term ~ ("%" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OMod, e1, e2)  
    } |
    term ~ ("@" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OAt, e1, e2)
    } | factor
  lazy val factor: Parser[Expr] =
    "+" ~> factor ^^ { case e => UnaryExpr(UPlus, e) } |
    "-" ~> factor ^^ { case e => UnaryExpr(UMinus, e) } |
    "~" ~> factor ^^ {case e => UnaryExpr(UInv, e) } |
    power 
  lazy val power: Parser[Expr] =
    awaitPrimary ~ ("**" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OPow, e1, e2)
    } | awaitPrimary
  lazy val awaitPrimary: Parser[Expr] =
    "await" ~> primary ^^ {
      case e => AwaitExpr(e)
    } | primary
  lazy val primary: Parser[Expr] =
    //invalidPrimary |
    primary ~ ("." ~> id) ^^ { case e ~ i => EAttrRef(e, i) } |
    primary ~ genexp ^^ {???} |
    primary ~ ("(" ~> opt(args) <~ ")") ^^ { 
      case f ~ None => Call(f, Args(List(), List(), List(), List()))
      case f ~ Some(args) => Call(f, args)
    } |
    primary ~ ("[" ~> slices <~ "]") ^^ { ??? } |
    atom
  // TODO slices
  lazy val slices: Parser[List[Expr]] =
    slice ~ not(",") ^^ { ??? } |
    repsep(slice, ",") ~ opt(",") ^^ { ??? }
  lazy val slice: Parser[Expr] =
    opt(expression) ~ (":" ~> opt(expression)) ~ opt(":" ~> opt(expression)) ^^ {
      ???
    } | namedExpr
  
  // atoms : literal-like production
  lazy val atom: Parser[Expr] =
    id  |
    "True" ^^^ ABool(true) |
    "False" ^^^  ABool(false) |
    strings |
    number |
    (tuple | group | genexp) |
    (list | listcomp) |
    (dict | set | dictcomp | setcomp)

  // TODO make primitive parser for these
  // TODO complete comprehensions
  lazy val strings: Parser[Expr] = stringLiteral
  lazy val list: Parser[Expr] = "[" ~> opt(starNamedExprs) <~ "]" ^^ {
    case Some(el) => ListDisplay(el) 
    case None => ListDisplay(List())
  }
  lazy val listcomp: Parser[Expr] = "[" ~> (namedExpr ~ forIfClauses) <~ "]" ^^ { 
    ??? 
  }  
  lazy val tuple: Parser[Expr] = "(" ~> opt(starNamedExpr ~ ("," ~> opt(starNamedExprs))) <~ ")" ^^ { 
    // 0 elem
    case None => TupleDisplay(List())
    // 1 elem 
    case Some(e ~ None) => TupleDisplay(List(e)) 
    // 2+ elem
    case Some(e ~ Some(el)) => TupleDisplay(e +: el)
  }
  lazy val group: Parser[Expr] = "(" ~> (yieldExpr | namedExpr) <~ ")" ^^ { ??? }
  lazy val genexp: Parser[Expr] = "(" ~> (assignExpr | expression - ":=") ~ forIfClauses <~ ")" ^^ { ??? }
  lazy val set: Parser[Expr] = "{" ~> starNamedExprs <~ "}" ^^ {
    case el => SetDisplay(el)
  }
  lazy val setcomp: Parser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ { ??? } 
  // TODO refactor this function
  lazy val dict: Parser[Expr] = 
    "{" ~> opt(doubleStarredKvpairs) <~ "}" ^^ {
      case None => DictDisplay(List(), List())
      case Some(kvs) => {
        val folder: ((List[(Expr, Expr)], List[Expr]), Expr) => ((List[(Expr, Expr)], List[Expr])) =
          (sum, elem) => elem match {
            case KVPair(k, v) => sum match { case (kvl, gl) => (kvl :+ (k, v), gl) }
            case e: Expr => sum match { case (kvl, gl) => (kvl, gl :+ e) }
          }
        val initKvl = List[(Expr, Expr)]()
        val initGl = List[Expr]()
        val (kvl, gl) = kvs.foldLeft( (initKvl, initGl) )(folder)
        DictDisplay(kvl, gl)    
      }
    }
    // | "{" ~> invalidDoudlbeStarredKvpairs <~ "}" ^^ { ??? }
  lazy val dictcomp: Parser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ { ??? }
  lazy val doubleStarredKvpairs: Parser[List[Expr]] = repsep(doubleStarredKvpair, ",") <~ opt(",")
  lazy val doubleStarredKvpair: Parser[Expr] = "**" ~> bitOr | kvPair 
  lazy val kvPair: Parser[KVPair] = expression ~ (":" ~> expression) ^^ {
    case e1 ~ e2 => KVPair(e1, e2)
  }
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
    "yield" ~> opt(starExprs) ^^ { ??? }
  
  //////////////////////////////////////////////////////////////////
  // arguments
  // Caution: very complex
  /////////////////////////////////////////////////////////////////
  lazy val arguments: Parser[Args] = args <~ (opt(",") ~ guard(")"))
  lazy val exprToArg: Parser[Arg] = (assignExpr | expression <~ not(":=")) ^^ {
    case e => PosArg(e)
  }
  // helper function for positional arguments case 
  def pargsToArgs: List[Arg] => Args = al => {
    val (pl, prest) = al.foldLeft( (List[PosArg](), List[PosRest]()) )( (sum, elem) => elem match {
      case a: PosArg => sum match { case (pl, prest) => (pl :+ a, prest) }
      case a: PosRest => sum match { case (pl, prest) => (pl, prest :+ a) }
      case _ => ??? // TODO raise exception, non-positional argument should not appear
    })
    Args(pl, prest, List(), List())
  }
  // helper function for keyword arguments case
  def kwargsToArgs: List[Arg] => Args = al => {
    val (kl, krest) = al.foldLeft( (List[KeyArg](), List[KeyRest]()) )( (sum, elem) => elem match {
      case a: KeyArg => sum match { case (kl, krest) => (kl :+ a, krest) }
      case a: KeyRest => sum match { case (kl, krest) => (kl, krest :+ a) }
      case _ => ??? // TODO raise exception, non-keyword argument should not appear
    })
    Args(List(), List(), kl, krest)  
  }
  lazy val args: Parser[Args] = 
    // positional and keywords arguments case
    repsep(starredExpr | exprToArg <~ not("="), ",") ~ opt("," ~> kwargs) ^^ {
      case el ~ None => pargsToArgs(el)
      case el ~ Some(kl) => {
        val pArgs = pargsToArgs(el)
        val kArgs = kwargsToArgs(kl)
        pArgs.copy(keyArgs=kArgs.keyArgs, keyRest=kArgs.keyRest)
      }
    } | kwargs ^^ kwargsToArgs // only keyword args case

  lazy val kwargs: Parser[List[Arg]] =
    repsep(kwargOrStarred, ",") ~ ("," ~> repsep(kwargOrDoubleStarred, ",")) ^^ {
      case l1 ~ l2 => l1 ++ l2 
    } |
    repsep(kwargOrStarred, ",") |
    repsep(kwargOrDoubleStarred, ",")
  lazy val starredExpr: Parser[PosRest] = "*" ~> expression ^^ { PosRest(_) }
  lazy val kwargOrStarred: Parser[Arg] =
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | starredExpr
  lazy val kwargOrDoubleStarred: Parser[Arg] = 
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | "**" ~> expression ^^ { KeyRest(_) }

  //////////////////////////////////////////////////////////////////

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

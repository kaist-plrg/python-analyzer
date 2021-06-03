package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  // TODO change Expr to Stmt
  def apply(tokens: List[Token]): Expr =
    expression(new PackratReader(TokenReader(tokens))).get
}
trait TokenListParsers extends PackratParsers {
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
  lazy val id: PackratParser[AId] = Parser(in => firstMap(in, _ match {
    case Id(name) => Success(AId(name), in.rest)
    case t => Failure(s"", in)
  }))

  // keywords, op and delimiters
  lazy val keyword: PackratParser[String] = Parser(in => firstMap(in, _ match {
    case Keyword(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val op: PackratParser[String] = Parser(in => firstMap(in, _ match {
    case Op(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val delim: PackratParser[String] = Parser(in => firstMap(in, _ match {
    case Delim(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  }))

  lazy val namedLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case Keyword(s) if s == ABool(true) => Success(ABool(true), in.rest)
    case Keyword(s) if s == ABool(false) => Success(ABool(false), in.rest)
    case Keyword(s) if s == ANone => Success(ANone, in.rest)
    case _ => Failure(s"", in)
  }))
  
  // literals, number, name(id)
  lazy val stringLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case StrLiteral(s) => Success(AStringLiteral(s), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val bytesLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case BytesLiteral(b) => Success(ABytesLiteral(b), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val intLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case IntLiteral(i) => Success(AIntLiteral(i.toInt), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val floatLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case FloatLiteral(f) => Success(AFloatLiteral(f.toDouble), in.rest)
    case t => Failure(s"", in) 
  }))

  lazy val imagLiteral: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case ImagLiteral(i) => Success(AImagLiteral(i.toDouble), in.rest)
    case t => Failure(s"", in)
  }))

  lazy val number: PackratParser[Expr] = intLiteral | floatLiteral | imagLiteral

  private def splitText(s: String): List[String] =
    "([a-zA-Z0-9_]+|\\S)".r.findAllIn(s).toList

  implicit def text(str: String): PackratParser[String] = {
    Parser(in => {
      firstMap(in, t => t match {
          case Op(s) if s == str => Success(s, in.rest)
          case Delim(s) if s == str => Success(s, in.rest)
          case Keyword(s) if s == str => Success(s, in.rest)
          case t => Failure(s"", in)
        })
    })
  }

  ///////////////////////////////////////////////
  // expressions
  ///////////////////////////////////////////////
  
  lazy val listOf = (p: Parser[Expr]) => p ~ repsep(p, ",") <~ opt(",") ^^ {
    case e ~ le => e :: le
  }
  lazy val starExprs: PackratParser[List[Expr]] = listOf(starExpr)
  lazy val starExpr: PackratParser[Expr] =
    "*" ~> bitOr | expression
  lazy val starNamedExprs: PackratParser[List[Expr]] = listOf(starNamedExpr)

  lazy val starNamedExpr: PackratParser[Expr] =
    "*" ~> bitOr | namedExpr
  lazy val assignExpr: PackratParser[Expr] = id ~ (":=" ~> commit(expression)) ^^ {
    case x ~ e => AssignExpr(x, e)
  }
  lazy val namedExpr: PackratParser[Expr] =
    assignExpr | expression - ":="
  lazy val expressions: PackratParser[List[Expr]] = listOf(expression)
  lazy val expression: PackratParser[Expr] =
    // lambdef |
    disjunction |
    disjunction ~ ("if" ~> disjunction ~ ("else" ~> expression)) ^^ {
      case ie ~ (te ~ ee) => CondExpr(ie, te, ee)
    }
  // lambda expressions
  lazy val lambdef: PackratParser[Expr] = ("lambda" ~> opt(lambdaParams <~ ":")) ~ expression ^^ {
    case Some(pl) ~ e => LambdaExpr(pl, e)
  }
  lazy val lambdaParams: PackratParser[List[Param]] = (
    lambdaSlashNoDefault ~ rep(lambdaParamNoDefault) ~ rep(lambdaParamWithDefault) ^^ {
      case lp1 ~ lp2 ~ lp3 => lp1 ++ lp2 ++ lp3
    } | lambdaSlashWithDefault ~ rep(lambdaParamWithDefault) ^^ {
      case lp1 ~ lp2 => lp1 ++ lp2
    } | rep1(lambdaParamNoDefault) ~ rep(lambdaParamWithDefault) ^^ {
      case lp1 ~ lp2 => lp1 ++ lp2
    } | rep1(lambdaParamWithDefault)
    ) ~ opt(lambdaStarEtc) ^^ {
      case lp ~ opt => opt.map(lp ++ _) getOrElse lp
    } | lambdaStarEtc
  lazy val lambdaSlashNoDefault: PackratParser[List[Param]] =
    rep1(lambdaParamNoDefault) <~ ("/" ~ ("," | guard(":")))
  lazy val lambdaSlashWithDefault: PackratParser[List[Param]] =
    rep(lambdaParamNoDefault) ~ rep1(lambdaParamWithDefault) <~ ("/" ~ ("," | guard(":"))) ^^ {
      case lp1 ~ lp2 => lp1 ++ lp2
    }
  lazy val lambdaStarEtc: PackratParser[List[Param]] = (
    "*" ~> (id <~ lambdaSep) ~ rep(lambdaParamMaybeDefault) ^^ {
      case pos ~ l => ArbPosParam(pos) :: l
    } | ("*" ~ ",") ~> rep1(lambdaParamMaybeDefault)
    ) ~ opt(lambdaKwds) ^^ {
      case lp ~ opt => opt.map(lp :+ _) getOrElse lp
    } | lambdaKwds ^^ { List(_) }
  lazy val lambdaKwds: PackratParser[Param] = "**" ~> id <~ lambdaSep ^^ ArbPosParam
  lazy val lambdaParamNoDefault: PackratParser[Param] = id <~ lambdaSep ^^ { NormalParam(_, None) }
  lazy val lambdaParamWithDefault: PackratParser[Param] = id ~ default <~ lambdaSep ^^ {
    case x ~ e => NormalParam(x, Some(e))
  }
  lazy val lambdaParamMaybeDefault: PackratParser[Param] = id ~ opt(default) <~ lambdaSep ^^ {
    case x ~ opt => NormalParam(x, opt)
  }
  lazy val lambdaSep = "," | guard(":")
  // lazy val lambdaParam: PackratParser[AId] = id

  // Expressions : production rules
  lazy val disjunction: PackratParser[Expr] =
    conjunction ~ rep1("or" ~> conjunction) ^^ {
      case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LOr, sum, elem) )
    } | conjunction
  lazy val conjunction: PackratParser[Expr] =
    inversion ~ rep1("and" ~> inversion) ^^ {
      case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LAnd, sum, elem) )
    } | inversion
  lazy val inversion: PackratParser[Expr] =
    "not" ~> inversion ^^ {
      case e => UnaryExpr(LNot, e)
    } | comparison
  lazy val comparison: PackratParser[Expr] = bitOr ~ rep1(compareOpBitOrPair) ^^ {
    case be ~ Nil => be
    case be ~ (h :: t) =>
      t.foldLeft((BinaryExpr(h._1, be, h._2), h._2)) ((tup, e) => {
        val (tempRes, lhs) = tup 
        val (op, rhs) = e 
        (BinaryExpr(LAnd, tempRes, BinaryExpr(op, lhs, rhs)), rhs)
      }   
    )._1
  }
  lazy val compareOpBitOrPair: PackratParser[(Op, Expr)] =
    eqBitOr | noteqBitOr | lteBitOr | ltBitOr | gteBitOr | gtBitOr | notinBitOr | inBitOr | isnotBitOr | isBitOr
  lazy val eqBitOr: PackratParser[(Op, Expr)] = "==" ~> bitOr ^^ { (CEq, _) }  
  lazy val noteqBitOr: PackratParser[(Op, Expr)] = "!=" ~> bitOr ^^ { (CNeq, _) }
  lazy val lteBitOr: PackratParser[(Op, Expr)] = "<=" ~> bitOr ^^ { (CLte, _) }
  lazy val ltBitOr: PackratParser[(Op, Expr)] = "<" ~> bitOr ^^ { (CLt, _) }
  lazy val gteBitOr: PackratParser[(Op, Expr)] = ">=" ~> bitOr ^^ { (CGte, _) }
  lazy val gtBitOr: PackratParser[(Op, Expr)] = ">" ~> bitOr ^^ { (CGt, _) }
  lazy val notinBitOr: PackratParser[(Op, Expr)] = ("not" ~ "in") ~> bitOr ^^ { (CNotIn, _) }
  lazy val inBitOr: PackratParser[(Op, Expr)] = "in" ~> bitOr ^^ { (CIn, _) }
  lazy val isnotBitOr: PackratParser[(Op, Expr)] = ("is" ~ "not") ~> bitOr ^^ { (CIsNot, _) }
  lazy val isBitOr: PackratParser[(Op, Expr)] = "is" ~> bitOr ^^ { (CIs, _) }

  lazy val bitOr: PackratParser[Expr] = bitOr ~ ("|" ~> bitXor) ^^ {
    case e1 ~ e2 => BinaryExpr(OBOr, e1, e2)
  } | bitXor
  lazy val bitXor: PackratParser[Expr] = bitXor ~ ("^" ~> bitAnd) ^^ {
    case e1 ~ e2 => BinaryExpr(OBXor, e1, e2) 
  } | bitAnd
  lazy val bitAnd: PackratParser[Expr] = bitAnd ~ ("&" ~> shiftExpr) ^^ {
    case e1 ~ e2 => BinaryExpr(OBAnd, e1, e2)
  } | shiftExpr
  lazy val shiftExpr: PackratParser[Expr] = 
    shiftExpr ~ ("<<" ~> sum) ^^ {
      case e1 ~ e2 => BinaryExpr(OLShift, e1, e2)
    } | 
    shiftExpr ~ (">>" ~> sum) ^^ {
      case e1 ~ e2 => BinaryExpr(ORShift, e1, e2)
    } | sum

  lazy val sum: PackratParser[Expr] = 
    sum ~ ("+" ~> term) ^^ {
      case e1 ~ e2 => BinaryExpr(OAdd, e1, e2)
    } |
    sum ~ ("-" ~> term) ^^ {
      case e1 ~ e2 => BinaryExpr(OSub, e1, e2)
    } | term
  lazy val term: PackratParser[Expr] =
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
  lazy val factor: PackratParser[Expr] =
    "+" ~> factor ^^ { case e => UnaryExpr(UPlus, e) } |
    "-" ~> factor ^^ { case e => UnaryExpr(UMinus, e) } |
    "~" ~> factor ^^ { case e => UnaryExpr(UInv, e) } |
    power 
  lazy val power: PackratParser[Expr] =
    awaitPrimary ~ ("**" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OPow, e1, e2)
    } | awaitPrimary
  lazy val awaitPrimary: PackratParser[Expr] =
    "await" ~> primary ^^ {
      case e => AwaitExpr(e)
    } | primary
  lazy val invalidPrimary: PackratParser[Expr] = Parser(in => firstMap(in, _ match {
    case _ => Failure(s"", in)
  }))
  lazy val primary: PackratParser[Expr] =
    // invalidPrimary |
    primary ~ ("." ~> id) ^^ { case e ~ i => EAttrRef(e, i) } |
    // primary ~ genexp ^^ {???} |
    primary ~ ("(" ~> opt(args) <~ ")") ^^ { 
      case f ~ None => Call(f, Args(List(), List(), List(), List()))
      case f ~ Some(args) => Call(f, args)
    } |
    primary ~ ("[" ~> slices <~ "]") ^^ {
      case p ~ sel => Slicing(p, sel)
    } |
    atom
  // TODO slices
  lazy val slices: PackratParser[List[Expr]] =
    slice <~ not(",") ^^ { case se => List(se) } |
    repsep(slice, ",") <~ opt(",") ^^ { case sel => sel }
  lazy val slice: PackratParser[Expr] =
    opt(expression) ~ (":" ~> opt(expression)) ~ opt(":" ~> opt(expression)) ^^ {
      case o1 ~ o2 ~ None => Slice(o1, o2, None)
      case o1 ~ o2 ~ Some(o3) => Slice(o1, o2, o3)
    } | namedExpr
  
  // atoms : literal-like production
  lazy val atom: PackratParser[Expr] =
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
  lazy val strings: PackratParser[Expr] = stringLiteral
  lazy val list: PackratParser[Expr] = "[" ~> opt(starNamedExprs) <~ "]" ^^ {
    case Some(el) => ListDisplay(el) 
    case None => ListDisplay(List())
  }
  lazy val listcomp: PackratParser[Expr] = "[" ~> (namedExpr ~ forIfClauses) <~ "]" ^^ { 
    case e ~ complist => ListCompExpr(e, complist) 
  }  
  lazy val tuple: PackratParser[Expr] = "(" ~> opt(starNamedExpr ~ ("," ~> opt(starNamedExprs))) <~ ")" ^^ { 
    // 0 elem
    case None => TupleDisplay(List())
    // 1 elem 
    case Some(e ~ None) => TupleDisplay(List(e)) 
    // 2+ elem
    case Some(e ~ Some(el)) => TupleDisplay(e +: el)
  }
  lazy val group: PackratParser[Expr] = "(" ~> (yieldExpr | namedExpr) <~ ")" ^^ {
    case e => GroupExpr(e)
  }
  lazy val genexp: PackratParser[Expr] = "(" ~> (assignExpr | expression - ":=") ~ forIfClauses <~ ")" ^^ {
    case e ~ cel => GenExpr(e, cel)  
  }
  lazy val set: PackratParser[Expr] = "{" ~> starNamedExprs <~ "}" ^^ {
    case el => SetDisplay(el)
  }
  lazy val setcomp: PackratParser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ {
    case e ~ complist => SetCompExpr(e, complist)
  } 
  // TODO refactor this function
  lazy val dict: PackratParser[Expr] = 
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
  lazy val dictcomp: PackratParser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ {
    case kv ~ complist => DictCompExpr(kv, complist) 
  }
  lazy val doubleStarredKvpairs: PackratParser[List[Expr]] = repsep(doubleStarredKvpair, ",") <~ opt(",")
  lazy val doubleStarredKvpair: PackratParser[Expr] = "**" ~> bitOr | kvPair 
  lazy val kvPair: PackratParser[KVPair] = expression ~ (":" ~> expression) ^^ {
    case e1 ~ e2 => KVPair(e1, e2)
  }
  // Comprehensions
  // 
  lazy val forIfClauses: PackratParser[List[Expr]] = rep(forIfClause) 
  // comp_for
  // this returns CompExpr
  lazy val forIfClause: PackratParser[Expr] =
    (opt("async") <~ "for") ~ starTargets ~ ("in" ~> disjunction ~ rep("if" ~> disjunction)) ^^ {
      case Some(_) ~ target ~ (inExpr ~ ifExprs) => CompExpr(target, inExpr, ifExprs, true)   
      case None ~ target ~ (inExpr ~ ifExprs) => CompExpr(target, inExpr, ifExprs, false)
    }
  lazy val yieldExpr: PackratParser[Expr] =
    ("yield" ~ "from") ~> expression ^^ { case e => YieldExpr(List(e))} |
    "yield" ~> opt(starExprs) ^^ { 
      case Some(el) => YieldExpr(el)
      case None => YieldExpr(List())
    }
  
  //////////////////////////////////////////////////////////////////
  // arguments
  // Caution: very complex
  /////////////////////////////////////////////////////////////////
  lazy val arguments: PackratParser[Args] = args <~ (opt(",") ~ guard(")"))
  lazy val exprToArg: PackratParser[Arg] = (assignExpr | expression <~ not(":=")) ^^ {
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
  lazy val args: PackratParser[Args] = 
    // positional and keywords arguments case
    repsep(starredExpr | exprToArg <~ not("="), ",") ~ opt("," ~> kwargs) ^^ {
      case el ~ None => pargsToArgs(el)
      case el ~ Some(kl) => {
        val pArgs = pargsToArgs(el)
        val kArgs = kwargsToArgs(kl)
        pArgs.copy(keyArgs=kArgs.keyArgs, keyRest=kArgs.keyRest)
      }
    } | kwargs ^^ kwargsToArgs // only keyword args case

  lazy val kwargs: PackratParser[List[Arg]] =
    repsep(kwargOrStarred, ",") ~ ("," ~> repsep(kwargOrDoubleStarred, ",")) ^^ {
      case l1 ~ l2 => l1 ++ l2 
    } |
    repsep(kwargOrStarred, ",") |
    repsep(kwargOrDoubleStarred, ",")
  lazy val starredExpr: PackratParser[PosRest] = "*" ~> expression ^^ { PosRest(_) }
  lazy val kwargOrStarred: PackratParser[Arg] =
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | starredExpr
  lazy val kwargOrDoubleStarred: PackratParser[Arg] = 
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | "**" ~> expression ^^ { KeyRest(_) }

  //////////////////////////////////////////////////////////////////

  // targets
  // TODO complete this (last part of full grammar)
  lazy val starTargets: PackratParser[List[Expr]] =  
    starTarget - "," ^^ { ??? } |
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTargetsListSeq: PackratParser[List[Expr]] =
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTargetTupleSeq: PackratParser[List[Expr]] =
    repsep(starTarget, ",") <~ opt(",") ^^ { ??? }
  lazy val starTarget: PackratParser[Expr] = ???
  // ...
  lazy val targets: PackratParser[List[Expr]] =
    repsep(target, ",") <~ opt(",") ^^ { ??? } 
  lazy val target: PackratParser[Expr] = ???

  ////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Statements
  lazy val statements: PackratParser[List[Stmt]] = rep(statement)
  lazy val statement: PackratParser[Stmt] = simpleStmt
  
  lazy val compoundStmt: PackratParser[Stmt] = ???
  lazy val simpleStmt: PackratParser[Stmt] = expressions ^^ { StarExprs(_) }
  lazy val suite: PackratParser[Stmt] = funcdef // TODO add others

  // TODO complete this
  lazy val funcdef: PackratParser[Stmt] =
    opt(decorators) ~ ("def" ~> id ~ ("(" ~> opt(paramList) <~ ")")) ~ (":" ~> suite) ^^ {
      // TODO add yes decorator case
      case None ~ (i ~ None) ~ s => ???
      case None ~ (i ~ Some(pl)) ~ s => ???
    }
  lazy val decorators: PackratParser[List[Expr]] = ??? 
  lazy val paramList: PackratParser[List[Param]] =
    defparam ~ ("," ~> defparam).* ~ (("," ~ "/") ~> opt("," ~> opt(paramListNoPosonly))) ^^ {
      // no posonly
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes posonly
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListNoPosonly

  lazy val paramListNoPosonly: PackratParser[List[Param]] =
    defparam ~ ("," ~> defparam).* ~ opt("," ~> opt(paramListStarargs)) ^^ {
      // no starparams
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes starparams
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListStarargs

  // parser for parameters appear after star or double-star
  lazy val paramListStarargs: PackratParser[List[Param]] =
    oneStarParam ~ ("," ~> defparam).* ~ opt("," ~> opt(doubleStarParam)) ^^ {
      // no kwargs
      case o ~ pl ~ None => o.toList ++ pl 
      case o ~ pl ~ Some(None) => o.toList ++ pl 
      // yes kwargs
      case o ~ pl ~ Some(Some(p)) => o.toList ++ pl ++ List(p) 
    } ^^ { pl => pl.map({
      case p: NormalParam => p
      case p: ArbPosParam => p
      case p: ArbKeyParam => p
    })} | 
    doubleStarParam <~ opt(",") ^^ { List(_) }

  // parser for arbitrary positional parameter
  lazy val oneStarParam: PackratParser[Option[Param]] = ("*" ~> opt(param)) ^^ {
    case Some(i) => Some(ArbPosParam(i))
    case None => None
  }
  // parser for arbitrary keyword parameter
  lazy val doubleStarParam: PackratParser[Param] = "**" ~> param <~ opt(",") ^^ {
    case i => ArbKeyParam(i) 
  }
  // parser for normal parameter
  lazy val defparam: PackratParser[Param] = param ~ ("=" ~> opt(expression)) ^^ {
    case i ~ oe => NormalParam(i, oe)
  }
  // parser for parameter id
  lazy val param: PackratParser[AId] = id // TODO add optional type expr `: expr`
  lazy val default: PackratParser[Expr] = "=" ~> expression
}

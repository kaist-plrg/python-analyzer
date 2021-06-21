package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  def apply(ts: Seq[Token]) = ???
}
trait TokenListParsers extends PackratParsers {
  ///////////////////////////////////////////////////////////////////
  // Basic Parsers definition, token reader
  ///////////////////////////////////////////////////////////////////
  type Elem = Token
  case class TokenPosition(line: Int, column: Int, protected val lineContents: String) extends Position
  class TokenReader(tokens: Seq[Token], pos: TokenPosition) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = pos
    override def rest: Reader[Token] = new TokenReader(tokens.tail, pos.copy(column=pos.column+1))
  }
  def TokenReader(ts: Seq[Token]) = new TokenReader(ts, TokenPosition(0,0, ""))

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
  
  lazy val starExprs: PackratParser[Expr] = starExpr ~ rep1("," ~> starExpr) <~ opt(",") ^^ {
    case e ~ le => TupleExpr(e :: le)
  } | starExpr <~ "," ^^ {
    case x => TupleExpr(List(x))
  } | starExpr
  lazy val starExpr: PackratParser[Expr] =
    ("*" ~> bitOr) ^^ StarExpr | expression
  lazy val starNamedExprs: PackratParser[List[Expr]] = rep1sep(starNamedExpr, ",") <~ opt(",")

  lazy val starNamedExpr: PackratParser[Expr] =
    ("*" ~> bitOr) ^^ StarExpr | namedExpr
  lazy val assignExpr: PackratParser[Expr] = id ~ (":=" ~> commit(expression)) ^^ {
    case x ~ e => AssignExpr(x, e)
  }
  lazy val namedExpr: PackratParser[Expr] =
    assignExpr | expression <~ not(":=")
  lazy val expressions: PackratParser[Expr] = expression ~ rep1("," ~> expression) <~ opt(",") ^^ {
    case e ~ le => TupleExpr(e :: le)
  } | expression <~ "," ^^ {
    case x => TupleExpr(List(x))
  } | expression
  lazy val expression: PackratParser[Expr] =
    disjunction ~ ("if" ~> disjunction ~ ("else" ~> expression)) ^^ {
      case ie ~ (te ~ ee) => CondExpr(ie, te, ee)
    } | disjunction | lambdef
  // lambda expressions
  lazy val lambdef: PackratParser[Expr] = ("lambda" ~> opt(lambdaParams <~ ":")) ~ expression ^^ {
    case Some(pl) ~ e => LambdaExpr(pl, e)
    case None ~ e => LambdaExpr(Nil, e)
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
  lazy val disjunction: PackratParser[Expr] = conjunction ~ rep1("or" ~> conjunction) ^^ {
    case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LOr, sum, elem) )
  } | conjunction
  lazy val conjunction: PackratParser[Expr] = inversion ~ rep1("and" ~> inversion) ^^ {
    case e ~ el => el.foldLeft(e)( (sum, elem) => BinaryExpr(LAnd, sum, elem) )
  } | inversion
  lazy val inversion: PackratParser[Expr] = "not" ~> inversion ^^ {
    case e => UnaryExpr(LNot, e)
  } | comparison
  lazy val comparison: PackratParser[Expr] = bitOr ~ rep1(compareOpBitOrPair) ^^ {
    case be ~ lp => CompareExpr(be, lp)
  } | bitOr
  lazy val compareOpBitOrPair: PackratParser[(COp, Expr)] = cop ~ bitOr ^^ {
    case op ~ be => (op, be)
  }
  lazy val cop = (
    "==" ^^^ CEq |
    "!=" ^^^ CNeq |
    "<=" ^^^ CLte |
    "<" ^^^ CLt |
    ">=" ^^^ CGte |
    ">" ^^^ CGt |
    "not" ~ "in" ^^^ CNotIn |
    "in" ^^^ CIn |
    "is" ~ "not" ^^^ CIsNot |
    "is" ^^^ CIs
  )

  lazy val bitOr: PackratParser[Expr] = bitOr ~ ("|" ~> bitXor) ^^ {
    case e1 ~ e2 => BinaryExpr(OBOr, e1, e2)
  } | bitXor
  lazy val bitXor: PackratParser[Expr] = bitXor ~ ("^" ~> bitAnd) ^^ {
    case e1 ~ e2 => BinaryExpr(OBXor, e1, e2) 
  } | bitAnd
  lazy val bitAnd: PackratParser[Expr] = bitAnd ~ ("&" ~> shiftExpr) ^^ {
    case e1 ~ e2 => BinaryExpr(OBAnd, e1, e2)
  } | shiftExpr
  lazy val shiftop = (
    "<<" ^^^ OLShift |
    ">>" ^^^ ORShift
  )
  lazy val shiftExpr: PackratParser[Expr] = shiftExpr ~ shiftop ~ sum ^^ {
    case e1 ~ op ~ e2 => BinaryExpr(op, e1, e2)
  } | sum

  lazy val sumop = (
    "+" ^^^ OAdd |
    "-" ^^^ OSub
  )
  lazy val sum: PackratParser[Expr] = sum ~ sumop ~ term ^^ {
    case e1 ~ op ~ e2 => BinaryExpr(op, e1, e2)
  } | term
  lazy val bop = (
    "*" ^^^ OMul |
    "/" ^^^ ODiv |
    "//" ^^^ OIDiv |
    "%" ^^^ OMod |
    "@" ^^^ OAt
  )
  lazy val term: PackratParser[Expr] = term ~ bop ~ factor ^^ {
    case e1 ~ op ~ e2 => BinaryExpr(op, e1, e2)
  } | factor
  lazy val uop = (
    "+" ^^^ UPlus |
    "-" ^^^ UMinus |
    "~" ^^^ UInv
  )
  lazy val factor: PackratParser[Expr] = uop ~ factor ^^ {
    case op ~ e => UnaryExpr(op, e)
  } | power
  lazy val power: PackratParser[Expr] =
    awaitPrimary ~ ("**" ~> factor) ^^ {
      case e1 ~ e2 => BinaryExpr(OPow, e1, e2)
    } | awaitPrimary
  lazy val awaitPrimary: PackratParser[Expr] =
    "await" ~> primary ^^ {
      case e => AwaitExpr(e)
    } | primary
  lazy val primary: PackratParser[Expr] =
    // invalidPrimary |
    primary ~ ("." ~> id) ^^ { case e ~ i => EAttrRef(e, i) } |
    primary ~ genexp ^^ {???} |
    primary ~ ("(" ~> opt(args) <~ ")") ^^ { 
      case f ~ None => Call(f, Args(List(), List(), List(), List()))
      case f ~ Some(args) => Call(f, args)
    } |
    primary ~ ("[" ~> slices <~ "]") ^^ {
      case p ~ s => ESubscript(p, s)
    } |
    atom
  // TODO slices
  lazy val slices: PackratParser[List[Expr]] = slice <~ not(",") ^^ { List(_) } |
    rep1sep(slice, ",") <~ opt(",")
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
    case Some(el) => ListExpr(el) 
    case None => ListExpr(List())
  }
  lazy val listcomp: PackratParser[Expr] = "[" ~> (namedExpr ~ forIfClauses) <~ "]" ^^ { 
    case e ~ complist => ListCompExpr(e, complist) 
  }  
  lazy val tuple: PackratParser[Expr] = "(" ~> opt(starNamedExpr ~ ("," ~> opt(starNamedExprs))) <~ ")" ^^ { 
    // 0 elem
    case None => TupleExpr(List())
    // 1 elem 
    case Some(e ~ None) => TupleExpr(List(e)) 
    // 2+ elem
    case Some(e ~ Some(el)) => TupleExpr(e +: el)
  }
  lazy val group: PackratParser[Expr] = "(" ~> (yieldExpr | namedExpr) <~ ")" ^^ GroupExpr
  lazy val genexp: PackratParser[Expr] = "(" ~> (assignExpr | expression <~ not(":=")) ~ forIfClauses <~ ")" ^^ {
    case e ~ cel => GenExpr(e, cel)  
  }
  lazy val set: PackratParser[Expr] = "{" ~> starNamedExprs <~ "}" ^^ {
    case el => SetExpr(el)
  }
  lazy val setcomp: PackratParser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ {
    case e ~ complist => SetCompExpr(e, complist)
  } 
  lazy val dict: PackratParser[Expr] =  ("{" ~> opt(doubleStarredKvpairs) <~ "}" ^^  {
    x => DictExpr(x.getOrElse(Nil))
  }) //| "{" ~> invalidDoubleStarredKvpairs <~ "}" 
  lazy val dictcomp: PackratParser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ {
    case kv ~ complist => DictCompExpr(kv, complist) 
  }
  lazy val doubleStarredKvpairs: PackratParser[List[(Expr, Expr)]] = rep1sep(doubleStarredKvpair, ",") <~ opt(",")
  lazy val doubleStarredKvpair: PackratParser[(Expr, Expr)] =
    "**" ~> bitOr ^^ { (EEmpty, _) } | kvPair
  lazy val kvPair: PackratParser[(Expr, Expr)] = expression ~ (":" ~> expression) ^^ {
    case e1 ~ e2 => (e1, e2)
  }
  // Comprehensions
  // 
  lazy val forIfClauses: PackratParser[List[CompFor]] = rep(forIfClause) 
  // comp_for
  // this returns CompExpr
  lazy val forIfClause: PackratParser[CompFor] =
    (opt("async") <~ "for") ~ starTargets ~ ("in" ~> disjunction ~ rep("if" ~> disjunction)) ^^ {
      case Some(_) ~ target ~ (inExpr ~ ifExprs) => CompFor(target, inExpr, ifExprs, true)   
      case None ~ target ~ (inExpr ~ ifExprs) => CompFor(target, inExpr, ifExprs, false)
    }
  lazy val yieldExpr: PackratParser[Expr] = ("yield" ~ "from") ~> expression ^^
    YieldFromExpr | "yield" ~> opt(starExprs) ^^ YieldExpr
  
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
  lazy val starredExpr: PackratParser[PosRest] = "*" ~> expression ^^ PosRest
  lazy val kwargOrStarred: PackratParser[Arg] =
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | starredExpr
  lazy val kwargOrDoubleStarred: PackratParser[Arg] = 
    id ~ ("=" ~> expression) ^^ { case i ~ e => KeyArg(i, e)} | "**" ~> expression ^^ KeyRest

  //////////////////////////////////////////////////////////////////

  // targets
  lazy val starTargets: PackratParser[Expr] =  starTarget <~ not(",") |
    rep1sep(starTarget, ",") <~ opt(",") ^^ TupleExpr
  lazy val starTargetsListSeq: PackratParser[List[Expr]] = rep1sep(starTarget, ",") <~ opt(",")
  lazy val starTargetsTupleSeq: PackratParser[List[Expr]] = starTarget ~ rep1("," ~> starTarget) <~ opt(",") ^^ {
    case st ~ ls => st :: ls
  } | starTarget <~ "," ^^ { List(_) }
  lazy val starTarget: PackratParser[Expr] = ("*" ~ not("*")) ~> starTarget ^^ StarExpr |
    targetWithStarAtom
  lazy val targetWithStarAtom: PackratParser[Expr] = tPrimary ~ ("." ~> id <~ not(tLookahead)) ^^ {
    case prim ~ x => EAttrRef(prim, x)
  } | tPrimary ~ ("[" ~> slices <~ "]" ~ not(tLookahead)) ^^ {
    case prim ~ s => ESubscript(prim, s)
  } | starAtom
  lazy val starAtom: PackratParser[Expr] = id|
    "(" ~> targetWithStarAtom <~ ")" |
    "(" ~> starTargetsTupleSeq <~ ")" ^^ TupleExpr |
    "[" ~> starTargetsListSeq <~ "]" ^^ ListExpr

  lazy val tPrimary: PackratParser[Expr] =
    tPrimary ~ ("." ~> id <~ not(tLookahead)) ^^ {
      case prim ~ x => EAttrRef(prim, x)
    } | tPrimary ~ ("[" ~> slices <~ "]" ~ not(tLookahead)) ^^ {
      case prim ~ s => ESubscript(prim, s)
    } | tPrimary ~ genexp <~ not(tLookahead) ^^ {
      case prim ~ gen => Call(prim, ???) //TODO update call
    } | tPrimary ~ ("(" ~> opt(arguments) <~ ")" ~ not(tLookahead)) ^^ {
      case prim ~ opt => Call(prim, opt.getOrElse(Args(Nil, Nil, Nil, Nil)))
    } | atom <~ not(tLookahead)

  lazy val tLookahead: PackratParser[String] = "(" | "[" | "."
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
  lazy val simpleStmt: PackratParser[Stmt] = expressions ^^ StarStmt
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

  // TODO: Add more production
  val prodMap = Map(
    "Group" -> group,
    "List" -> list,
    "Tuple" -> tuple,
    "Set" -> set,
    "Atom" -> atom,
    "Primary" -> primary,
    "AwaitPrimary" -> awaitPrimary,
    "Power" -> power,
    // "Uop" -> uop,
    "Factor" -> factor,
    // "Bop" -> bop,
    "Term" -> term,
    // "Sop" -> uop,
    "Sum" -> sum,
    "ShiftExpr" -> shiftExpr,
    "BitAnd" -> bitAnd,
    "BitXor" -> bitXor,
    "BitOr" -> bitOr,
    // "Cop" -> cop,
    "Comparison" -> comparison,
    "Inversion" -> inversion,
    "Conjunction" -> conjunction,
    "Disjunction" -> disjunction,
    "Expression" -> expression,
    "Expressions" -> expressions,
    "NamedExpr" -> namedExpr,
    "AssignExpr" -> assignExpr,
    "StarNamedExpr" -> starNamedExpr,
    // StarNamedExprs is intermediate production which doesn't produce AST
    // "StarNamedExprs" -> starNamedExprs,
    "StarExpr" -> starExpr,
    "StarExprs" -> starExprs,
    "YieldExpr" -> yieldExpr,
  )


  /////////////////////////////////
  // Invalid productions
  ////////////////////////////////
  // invalid productions accepts come ill-formed subexpr and raise syntax erorr early
  def error(msg: String): Parser[Nothing] = Parser(in => firstMap(in, _ => Error(msg, in)))
  lazy val invalidPrimary: Parser[Nothing] = (primary ~ "{").into(_ => error("invalid syntax"))
  lazy val invalidDoubleStarredKvpairs: Parser[Nothing] =
    ( repsep(doubleStarredKvpair, ",") ~ "," ~ invalidKvpair
      | expression ~ ":" ~ "*" ~ bitOr
      | expression ~ ":" ~ guard("}"|",") 
    ).into(_ => error("invalid syntax")) //TODO : appropriate errormessage
  lazy val invalidKvpair: Parser[Nothing] =
    ( not(":")
      | expression ~ ":" ~ "*" ~ bitOr
      | expression ~ ":"
    ).into(_ => error("invalid syntax")) //TODO : appropriate error msg
}

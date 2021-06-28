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

  lazy val indent: PackratParser[Unit] = Parser(in => firstMap(in, _ match {
    case Indent => Success((), in.rest)
    case _ => Failure(s"", in)
  }))
  lazy val dedent: PackratParser[Unit] = Parser(in => firstMap(in, _ match {
    case Dedent => Success((), in.rest)
    case _ => Failure(s"", in)
  }))

  // TODO need impl. is this parser or tokenizer?
  lazy val typeComment: PackratParser[Unit] = ???

  lazy val number: PackratParser[Expr] = intLiteral | floatLiteral | imagLiteral

  private def splitText(s: String): List[String] =
    "([a-zA-Z0-9_]+|\\S)".r.findAllIn(s).toList

  implicit def text(str: String): PackratParser[String] = {
    Parser(in => {
      firstMap(in, t => t match {
          case Newline if str == "\n" => Success(s"\n", in.rest) 
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
  lazy val annotatedRhs: PackratParser[Expr] = yieldExpr | starExprs
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
  lazy val lambdef: PackratParser[Expr] = ("lambda" ~> opt(lambdaParams) <~ ":") ~ expression ^^ {
    case Some(pl) ~ e => LambdaExpr(pl, e)
    case None ~ e => LambdaExpr(Nil, e)
  }
  // Spec used Params expression for List of Param
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
    } | ("*" ~ ",") ~> rep1(id ~ opt(default) <~ lambdaSep) ^^ {
      case lx => lx.map {
        case x ~ opt => KeyParam(x, opt)
      }
    }) ~ opt(lambdaKwds) ^^ {
      case lp ~ opt => opt.map(lp :+ _) getOrElse lp
    } | lambdaKwds ^^ { List(_) }
  lazy val lambdaKwds: PackratParser[Param] = "**" ~> id <~ lambdaSep ^^ ArbKeyParam
  lazy val lambdaParamNoDefault: PackratParser[Param] = id <~ lambdaSep ^^ { PosParam(_, None) }
  lazy val lambdaParamWithDefault: PackratParser[Param] = id ~ default <~ lambdaSep ^^ {
    case x ~ e => PosParam(x, Some(e))
  }
  lazy val lambdaParamMaybeDefault: PackratParser[Param] = id ~ opt(default) <~ lambdaSep ^^ {
    case x ~ opt => PosParam(x, opt)
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
    // //invalidPrimary |
    primary ~ ("." ~> id) ^^ { case e ~ i => EAttrRef(e, i) } |
    primary ~ genexp ^^ {
      case f ~ g => Call(f, List(NormalArg(g)))
    } | primary ~ ("(" ~> opt(args) <~ ")") ^^ {
      case f ~ opt => Call(f, opt.getOrElse(Nil))
    } |
    primary ~ ("[" ~> slices <~ "]") ^^ {
      case p ~ s => ESubscript(p, s)
    } |
    atom
  // Note that spec returns tuple of Expr
  // We change it to List of Epxr for the consistency of parsing beautified AST
  lazy val slices: PackratParser[List[Expr]] = slice <~ not(",") ^^ { List(_) } |
    rep1sep(slice, ",") <~ opt(",")
  lazy val slice: PackratParser[Expr] =
    opt(expression) ~ (":" ~> opt(expression)) ~ opt(":" ~> opt(expression)) ^^ {
      case o1 ~ o2 ~ opt => Slice(o1, o2, opt.getOrElse(None))
    } | namedExpr
  
  // atoms : literal-like production
  lazy val atom: PackratParser[Expr] =
    id |
    "True" ^^^ ABool(true) |
    "False" ^^^  ABool(false) |
    "None" ^^^ ANone | 
    strings |
    number |
    (tuple | group | genexp) |
    (list | listcomp) |
    (dict | set | dictcomp | setcomp)

  // TODO make primitive parser for these
  lazy val strings: PackratParser[Expr] = stringLiteral
  
  // Displays (plain & comprehension)
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
  lazy val genexp: PackratParser[Expr] = "(" ~> ((assignExpr | expression <~ not(":=")) ~ forIfClauses) <~ ")" ^^ {
    case e ~ cel => GenExpr(e, cel)  
  }
  lazy val set: PackratParser[Expr] = "{" ~> starNamedExprs <~ "}" ^^ SetExpr
  lazy val setcomp: PackratParser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ {
    case e ~ complist => SetCompExpr(e, complist)
  } 
  lazy val dict: PackratParser[Expr] =  ("{" ~> opt(doubleStarredKvPairs) <~ "}" ^^  {
    x => DictExpr(x.getOrElse(Nil))
  }) | "{" ~> invalidDoubleStarredKvPairs <~ "}" 
  lazy val dictcomp: PackratParser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ {
    case kv ~ complist => DictCompExpr(kv, complist) 
  }
  lazy val doubleStarredKvPairs: PackratParser[List[DictItem]] = rep1sep(doubleStarredKvPair, ",") <~ opt(",")
  lazy val doubleStarredKvPair: PackratParser[DictItem] =
    "**" ~> bitOr ^^ DStarExpr ^^ DStarItem | kvPair
  lazy val kvPair: PackratParser[DictItem] = expression ~ (":" ~> expression) ^^ {
    case e1 ~ e2 => KvPair(e1, e2)
  }

  // Comprehensions
  lazy val forIfClauses: PackratParser[List[CompFor]] = rep1(forIfClause) 
  lazy val forIfClause: PackratParser[CompFor] = // Note. forIfClause same with comp_for
    (opt("async") <~ "for") ~ starTargets ~ ("in" ~> disjunction ~ rep("if" ~> disjunction)) ^^ {
      case Some(_) ~ target ~ (inExpr ~ ifExprs) => CompFor(target, inExpr, ifExprs, true)   
      case None ~ target ~ (inExpr ~ ifExprs) => CompFor(target, inExpr, ifExprs, false)
    }
  lazy val yieldExpr: PackratParser[Expr] = 
    ("yield" ~ "from") ~> expression ^^ YieldFromExpr |
    "yield" ~> opt(starExprs) ^^ YieldExpr
    // TODO: scala operator precedance problem here with | 

  //////////////////////////////////////////////////////////////////
  // arguments
  /////////////////////////////////////////////////////////////////
  lazy val arguments: PackratParser[List[Arg]] = args <~ (opt(",") ~ guard(")"))
  lazy val args: PackratParser[List[Arg]] =  rep1sep(starredExpr |
    (assignExpr | expression <~ not(":=")) <~ not("="), ",") ~ opt("," ~> kwargs) ^^ {
      case el ~ opt => el.map(NormalArg) ++ opt.getOrElse(Nil)
    } | kwargs
  lazy val kwargs: PackratParser[List[Arg]] =
    rep1sep(kwargOrStarred, ",") ~ rep("," ~> kwargOrDoubleStarred) ^^ {
      case l1 ~ l2 => l1 ++ l2
    } | rep1sep(kwargOrDoubleStarred, ",")
  lazy val starredExpr: PackratParser[Expr] = "*" ~> expression ^^ StarExpr
  lazy val kwargOrStarred: PackratParser[Arg] = id ~ ("=" ~> expression) ^^ {
    case i ~ e => KeyArg(i, e)
  } | starredExpr ^^ NormalArg
  lazy val kwargOrDoubleStarred: PackratParser[Arg] = id ~ ("=" ~> expression) ^^ {
    case i ~ e => KeyArg(i, e)
  } | "**" ~> expression ^^ DStarExpr ^^ NormalArg

  //////////////////////////////////////////////////////////////////
  // targets
  //////////////////////////////////////////////////////////////////
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
  //TODO need impl
  lazy val singleTarget: PackratParser[Expr] = (
    singleSubscriptAttrTarget | id | ("(" ~ singleTarget ~ ")")
  ) ^^ ???
  lazy val singleSubscriptAttrTarget: PackratParser[Expr] = (
    tPrimary ~ "." ~ id ~ not(tLookahead)
    | tPrimary ~ "[" ~ slices ~ "]" ~ not(tLookahead)
    | starAtom
  ) ^^ ???
  lazy val delTargets: PackratParser[List[Expr]] = (rep1sep(delTarget, ",") <~ opt(",")) ^^ ???
  lazy val delTarget: PackratParser[Expr] = (
    tPrimary ~ "." ~ id ~ not(tLookahead)
    | tPrimary ~ "[" ~ slices ~ "]" ~ not(tLookahead)
    | deltAtom
  ) ^^ ???
  lazy val deltAtom: PackratParser[Expr] = (
    id
    | "(" ~ delTarget ~ ")"
    | "(" ~ opt(delTargets) ~ ")"
    | "[" ~ opt(delTargets) ~ "]"
  ) ^^ ???

  lazy val tPrimary: PackratParser[Expr] =
    tPrimary ~ ("." ~> id <~ not(tLookahead)) ^^ {
      case prim ~ x => EAttrRef(prim, x)
    } | tPrimary ~ ("[" ~> slices <~ "]" ~ not(tLookahead)) ^^ {
      case prim ~ s => ESubscript(prim, s)
    } | tPrimary ~ genexp <~ not(tLookahead) ^^ {
      case prim ~ gen => Call(prim, List(NormalArg(gen))) //TODO update call
    } | tPrimary ~ ("(" ~> opt(arguments) <~ ")" ~ not(tLookahead)) ^^ {
      case prim ~ opt => Call(prim, opt.getOrElse(Nil))
    } | atom <~ not(tLookahead)

  lazy val tLookahead: PackratParser[String] = "(" | "[" | "."
  // ...
  lazy val targets: PackratParser[List[Expr]] =
    repsep(target, ",") <~ opt(",") ^^ { ??? } 
  lazy val target: PackratParser[Expr] = ???

  ////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Statements
  lazy val statements: PackratParser[List[Stmt]] = rep1(statement)
  lazy val statement: PackratParser[Stmt] = compoundStmt | simpleStmt
  lazy val statementNewline: PackratParser[Stmt] = 
    (compoundStmt <~ "\n") | simpleStmts ^^ ??? | ("\n" ^^^ EmptyStmt) //| endmarker  //TODO ad rule for endmarker  
  
  lazy val simpleStmtOne: PackratParser[List[Stmt]] = 
    (simpleStmt <~ (not(";") ~ "\n")) ^^ { case s: Stmt => List(s) } // TODO why cannot write it on below directly?
  lazy val simpleStmts: PackratParser[List[Stmt]] = 
    simpleStmtOne | (rep1sep(simpleStmt, ";") <~ (opt(";") ~ "\n"))

  lazy val simpleStmt: PackratParser[Stmt] =
    assignment | (starExprs ^^ StarStmt) | returnStmt | importStmt | raiseStmt | passStmt |
    delStmt | yieldStmt | assertStmt | breakStmt | continueStmt | globalStmt | nonlocalStmt |
    (expressions ^^ StarStmt)

  lazy val compoundStmt: PackratParser[Stmt] =
    funcDef | ifStmt | classDef | withStmt | forStmt | tryStmt | whileStmt | matchStmt

  // assignment stmt
  lazy val assignment: PackratParser[Stmt] =
    // TODO `":" ~> expression` part means type expression annotation... deal with this
    (id ~ (":" ~> expression) ~ opt("=" ~ annotatedRhs) ^^ {  
      case x ~ e ~ Some(rhs) => ??? 
      case x ~ e ~ None => ???
    }) |
    ( (("(" ~> singleTarget <~ ")") | singleSubscriptAttrTarget) <~ ":" ~ expression ~ opt("=" ~ annotatedRhs) ) ^^ { case _ => ??? } |
    (rep1(starTargets <~ "=") ~ (yieldExpr | starExprs) ~ (not("=") ~> opt(typeComment))) ^^ {
      case stl ~ e ~ _ => AssignStmt(stl, e) // TODO currently ignoring type comment
    } | 
    (singleTarget ~ augAssign ~ (yieldExpr | starExprs)) ^^ {
      case t ~ op ~ e => AugAssignStmt(t, op, e)
    } 
  lazy val augAssign: PackratParser[AugOp] = 
    ("+=" | "-=" | "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=") ^^ AugOp
 
  // some simple stmt
  lazy val globalStmt: PackratParser[Stmt] = ("global" ~> rep1sep(id, ",")) ^^ GlobalStmt 
  lazy val nonlocalStmt: PackratParser[Stmt] = ("nonlocal" ~> rep1sep(id, ",")) ^^ NonlocalStmt
  lazy val yieldStmt: PackratParser[Stmt] = yieldExpr ^^ YieldStmt
  lazy val assertStmt: PackratParser[Stmt] = ("assert" ~> expression ~ opt("," ~> expression)) ^^ {
    case e ~ opt => AssertStmt(e, opt)
  }
  lazy val delStmt: PackratParser[Stmt] = ("del" ~> delTargets <~ guard(";" | "\n")) ^^ {
    case tl => DelStmt(tl)
  }
  lazy val passStmt: PackratParser[Stmt] = "pass" ^^^ PassStmt
  lazy val breakStmt: PackratParser[Stmt] = "break" ^^^ BreakStmt
  lazy val continueStmt: PackratParser[Stmt] = "continue" ^^^ ContinueStmt

  // import stmt related
  lazy val importStmt: PackratParser[Stmt] = (importName | importFrom)
  lazy val importName: PackratParser[Stmt] = ("import" ~ dottedAsNames) ^^ ???
  lazy val importFrom: PackratParser[Stmt] = ( 
    "from" ~ rep("." | "...") ~ dottedName ~ "import" ~ importFromTargets
    | "from" ~ rep1("." | "...") ~ "import" ~  importFromTargets
  ) ^^ ???
  lazy val importFromTargets: PackratParser[Stmt] = (
    "(" ~ importFromAsNames ~ opt(",") ~ ")"
    | importFromAsNames  ~ not(",")
    | "*"
  ) ^^ ???
  lazy val importFromAsNames: PackratParser[Stmt] =
    rep1sep(importFromAsName, ",") ^^ ???
  lazy val importFromAsName: PackratParser[Stmt] = 
    (id ~ opt("as" ~ id)) ^^ ???
  lazy val dottedAsNames: PackratParser[Stmt] = 
    rep1sep(dottedAsName, ",") ^^ ???
  lazy val dottedAsName: PackratParser[Stmt] = 
    (dottedName ~ opt("as" ~ id)) ^^ ???
  lazy val dottedName: PackratParser[Stmt] = (
    dottedName ~ "." ~ id
    | id
  ) ^^ ???

  // control-flow stmt relate (if, while, for, with)
  lazy val ifStmt: PackratParser[Stmt] = (
    "if" ~ namedExpr ~ ":" ~ block ~ elifStmt
    | "if" ~ namedExpr ~ ":" ~ block ~ opt(elseBlock)
  ) ^^ ???
  lazy val elifStmt: PackratParser[Stmt] = (
    "elif" ~ namedExpr ~ ":" ~ block ~ elifStmt
    | "elif" ~ namedExpr ~ ":" ~ block ~ opt(elseBlock)
  ) ^^ ???
  lazy val elseBlock: PackratParser[Stmt] =
    ("else" ~ ":" ~ block) ^^ ???
  lazy val whileStmt: PackratParser[Stmt] = 
    ("while" ~ namedExpr ~ ":" ~ block ~ opt(elseBlock)) ^^ ???
  lazy val forStmt: PackratParser[Stmt] =
    (opt("async") ~ "for" ~ starTargets ~ "in" ~ starExprs ~ ":" ~ opt(typeComment) ~ block ~ opt(elseBlock)) ^^ ???
  lazy val withStmt: PackratParser[Stmt] = (
    opt("async") ~ "with" ~ "(" ~ rep1sep(withItem, ",") ~ opt(",") ~ ")" ~ ":" ~ block
    | opt("async") ~ "with" ~ rep1sep(withItem, ",") ~ ":" ~ opt(typeComment) ~ block
  ) ^^ ???
  lazy val withItem: PackratParser[Stmt] = (
    expression ~ "as" ~ starTarget ~ guard("," | ")" | ":")
    | expression
  ) ^^ ???
  
  // try-except stmt
  lazy val tryStmt: PackratParser[Stmt] = (
    "try" ~ ":" ~ block ~ finallyBlock
    | "try" ~ ":" ~ block ~ rep1(exceptBlock) ~ opt(elseBlock) ~ opt(finallyBlock)
  ) ^^ ???
  lazy val exceptBlock: PackratParser[Stmt] = (
    "except" ~ expression ~ opt("as" ~ id) ~ ":" ~ block
    | "except" ~ ":" ~ block
  ) ^^ ???
  lazy val finallyBlock: PackratParser[Stmt] = (
    "finally" ~ ":" ~ block
  ) ^^ ???

  // match stmt
  lazy val matchStmt: PackratParser[Stmt] =
    ("match" ~ subjectExpr ~ ":" ~ "\n" ~ indent ~ rep1(caseBlock) ~ dedent) ^^ ??? //TODO make parser for indent & dedent
  lazy val subjectExpr: PackratParser[Stmt] = (
    starNamedExpr ~ "," ~ opt(starNamedExprs)
    | namedExpr
  ) ^^ ???
  lazy val caseBlock: PackratParser[Stmt] =
    ("case" ~ patterns ~ opt("if" ~ namedExpr) ~ ":" ~ block) ^^ ???

  // match patterns
  lazy val patterns: PackratParser[Stmt] = ??? // TODO impl patterns

  // return, raise stmt
  lazy val returnStmt: PackratParser[Stmt] = 
    ("return" ~ opt(starExprs)) ^^ ???
  lazy val raiseStmt: PackratParser[Stmt] = (
    "raise" ~ expression ~ opt("from" ~ expression)
    | "raise"
  ) ^^ ???

  // function_def
  // TODO complete this
  lazy val suite: PackratParser[Stmt] = funcDef // TODO where this come from?
  lazy val funcDef: PackratParser[Stmt] =
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
      case p: PosParam => p
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
    case i ~ oe => PosParam(i, oe)
  }
  // parser for parameter id
  lazy val param: PackratParser[AId] = id // TODO add optional type expr `: expr`
  lazy val default: PackratParser[Expr] = "=" ~> expression

  // class def
  lazy val classDef: PackratParser[Stmt] = (
    decorators ~ classDefRaw
    |  classDefRaw
  ) ^^ ???
  lazy val classDefRaw: PackratParser[Stmt] =
    ("class" ~ id ~ opt("(" ~ args ~ ")") ~ ":" ~ block) ^^ ???

  // block
  lazy val block: PackratParser[Stmt] = (
    "\n" ~ indent ~ statements ~ dedent
    | simpleStmts
  ) ^^ ???

  /////////////////////////////////
  // Invalid productions
  ////////////////////////////////
  // invalid productions accepts come ill-formed subexpr and raise syntax erorr early
  def error(msg: String): Parser[Nothing] = Parser(in => firstMap(in, _ => Error(msg, in)))

  lazy val invalidPrimary: Parser[Nothing] = (primary ~ "{").into(_ => error("invalid syntax"))
  lazy val invalidDoubleStarredKvPairs: Parser[Nothing] =
    ( repsep(doubleStarredKvPair, ",") ~ "," ~ invalidKvPair
      | expression ~ ":" ~ "*" ~ bitOr
      | expression ~ ":" ~ guard("}"|",") 
    ).into(_ => error("invalid syntax")) //TODO : appropriate errormessage
  lazy val invalidKvPair: Parser[Nothing] =
    ( not(":")
      | expression ~ ":" ~ "*" ~ bitOr
      | expression ~ ":"
    ).into(_ => error("invalid syntax")) //TODO : appropriate error msg

  ///////////////////////////////
  // prodMap: mapping list of all productions
  ///////////////////////

  // TODO seperate prodMap to other
  // TODO: Add more production
  val prodMap: Map[String, Parser[Node]] = Map(
    "Group" -> group,
    "List" -> list,
    // "Listcomp" -> listcomp,
    "Tuple" -> tuple,
    "Set" -> set,
    // "Setcomp" -> setcomp,
    "Dict" -> dict,
    // "Dictcomp" -> dictcomp,
    // "ForIfClause" -> forIfClause,
    // "StarTargets" -> starTargets,
    // "StarTarget" -> starTarget,
    // "TargetWithStarAtom" -> targetWithStarAtom,
    // "StarAtom" -> starAtom,
    // "TPrimary" -> tPrimary,
    "Atom" -> atom,
    "Slice" -> slice,
    "Primary" -> primary,
    "AwaitPrimary" -> awaitPrimary,
    "Power" -> power,
    "Factor" -> factor,
    "Term" -> term,
    "Sum" -> sum,
    "ShiftExpr" -> shiftExpr,
    "BitAnd" -> bitAnd,
    "BitXor" -> bitXor,
    "Comparison" -> comparison,
    "Inversion" -> inversion,
    "Conjunction" -> conjunction,
    "Disjunction" -> disjunction,
    // Param
    // Lambda
    "Lambdef" -> lambdef,
    // Expression
    "Expression" -> expression,
    "Expressions" -> expressions,
    "NamedExpr" -> namedExpr,
    "AssignExpr" -> assignExpr,
    "StarNamedExpr" -> starNamedExpr,
    "StarExpr" -> starExpr,
    "StarExprs" -> starExprs,
    "YieldExpr" -> yieldExpr,
  )
}

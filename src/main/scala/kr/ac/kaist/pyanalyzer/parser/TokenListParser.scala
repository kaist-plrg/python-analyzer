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
    override def rest: Reader[Token] = new TokenReader(tokens.tail, 
      pos.copy(column=pos.column + tokens.head.toString.length() + 1)
    )
  }
  def TokenReader(ts: Seq[Token]) = 
    new TokenReader(ts, TokenPosition(0,0, ts.map(_.toString).mkString(" ").replaceAll("[\n\r]$", "")))

  //////////////////////////////////////////////
  // logging function
  // all productions must be explicitly wrapped with this
  ////////////////////////////////////////////
  var setLog = false
  
  def log[T](p: Parser[T])(name: String): Parser[T] = {
    if (!setLog) p else Parser{in: Input => { 
        val msg = (
          s"trying $name at [${in.pos}] \n" +
          s"${in.pos.longString}\n"
        )
        stop(msg) match {
          case "q" => 
            setLog = false
            p(in)
          case "j" =>
            setLog = false
            val r = p(in)
            println(name + " --> " + r)
            setLog = true
            r
          case _ =>
            val r = p(in)
            println(name + " --> " + r)
            r
        }
      }
    }
  }
  protected def stop(msg: String): String = {
     println(msg)
     val res = scala.io.StdIn.readLine
     res
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
  lazy val id: PackratParser[Id] = log(Parser(in => firstMap(in, _ match {
    case IdToken(name) => Success(Id(name), in.rest)
    case t => Failure(s"", in)
  })))("id")

  // keywords, op and delimiters
  lazy val keyword: PackratParser[String] = log(Parser(in => firstMap(in, _ match {
    case KeywordToken(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  })))("keyword")

  lazy val op: PackratParser[String] = log(Parser(in => firstMap(in, _ match {
    case OpToken(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  })))("op")

  lazy val delim: PackratParser[String] = log(Parser(in => firstMap(in, _ match {
    case DelimToken(s) => Success(s, in.rest)
    case t => Failure(s"", in)
  })))("delim")
  
  // literals, number, name(id)
  lazy val stringLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
    case StrToken(s) => Success(EConst(StringLiteral(s)), in.rest)
    case t => Failure(s"", in)
  })))("stringLiteral")

// TODO: Add bytesLiteral model
//  lazy val bytesLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
//    case BytesLiteral(b) => Success(ABytesLiteral(b), in.rest)
//    case t => Failure(s"", in)
//  })))("bytesLiteral")

  lazy val intLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
    case IntToken(i) => Success(EConst(IntLiteral(i.toInt)), in.rest)
    case t => Failure(s"", in)
  })))("intLiteral")

  lazy val floatLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
    case FloatToken(f) => Success(EConst(FloatLiteral(f.toDouble)), in.rest)
    case t => Failure(s"", in) 
  })))("floatLiteral")

  lazy val imagLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
    case ImagToken(i) => Success(EConst(ComplexLiteral(i.toDouble)), in.rest)
    case t => Failure(s"", in)
  })))("imagLiteral")

  lazy val indent: PackratParser[Unit] = log(Parser(in => firstMap(in, _ match {
    case IndentToken => Success((), in.rest)
    case _ => Failure(s"", in)
  })))("indent")
  lazy val dedent: PackratParser[Unit] = log(Parser(in => firstMap(in, _ match {
    case DedentToken => Success((), in.rest)
    case _ => Failure(s"", in)
  })))("dedent")

  // TODO need impl. is this parser or tokenizer?
  lazy val typeComment: PackratParser[TyComment] = ???

  lazy val number: PackratParser[Expr] = intLiteral | floatLiteral | imagLiteral

  private def splitText(s: String): List[String] =
    "([a-zA-Z0-9_]+|\\S)".r.findAllIn(s).toList

  implicit def text(str: String): PackratParser[String] = {
    Parser(in => {
      firstMap(in, t => t match {
          case NewlineToken if str == "\n" => Success(s"\n", in.rest) 
          case OpToken(s) if s == str => Success(s, in.rest)
          case DelimToken(s) if s == str => Success(s, in.rest)
          case KeywordToken(s) if s == str => Success(s, in.rest)
          case t => Failure(s"", in)
        })
    })
  }

  ///////////////////////////////////////////////
  // expressions
  ///////////////////////////////////////////////
  
  lazy val starExprs: PackratParser[Expr] =
    starExpr ~ rep1("," ~> starExpr) <~ opt(",") ^^ {
      case e ~ le => TupleExpr(e :: le)
    } | starExpr <~ "," ^^ (e => TupleExpr(List(e))) | starExpr
  lazy val starExpr: PackratParser[Expr] = ("*" ~> bitOr) ^^ Starred | expression
  lazy val starNamedExprs: PackratParser[List[Expr]] =
    rep1sep(starNamedExpr, ",") <~ opt(",")

  lazy val starNamedExpr: PackratParser[Expr] =
    ("*" ~> bitOr) ^^ Starred | namedExpr
  lazy val assignExpr: PackratParser[Expr] = id ~ (":=" ~> commit(expression)) ^^ {
    case x ~ e => NamedExpr(EName(x), e)
  }
  lazy val namedExpr: PackratParser[Expr] = assignExpr | expression <~ not(":=")
  lazy val annotatedRhs: PackratParser[Expr] = yieldExpr | starExprs
  lazy val expressions: PackratParser[Expr] =
    expression ~ rep1("," ~> expression) <~ opt(",") ^^ {
      case e ~ le => TupleExpr(e :: le)
    } | expression <~ "," ^^ { l => TupleExpr(List(l)) } | expression
  lazy val expression: PackratParser[Expr] = log(
    disjunction ~ ("if" ~> disjunction ~ ("else" ~> expression)) ^^ {
      case ie ~ (te ~ ee) => IfExpr(ie, te, ee)
    } | disjunction// | lambdef
  )("expression")
  // lambda expressions
  lazy val lambdef: PackratParser[Expr] = log(("lambda" ~> opt(lambdaParams) <~ ":") ~ expression ^^ {
    case Some(pl) ~ e => LambdaExpr(pl, e)
    case None ~ e => LambdaExpr(Nil, e)
  })("lambdef")
  // Spec used Params expression for List of Param
  lazy val lambdaParams: PackratParser[List[_]] = (
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
  lazy val lambdaSlashNoDefault: PackratParser[List[_]] =
    rep1(lambdaParamNoDefault) <~ ("/" ~ ("," | guard(":")))
  lazy val lambdaSlashWithDefault: PackratParser[List[_]] =
    rep(lambdaParamNoDefault) ~ rep1(lambdaParamWithDefault) <~ ("/" ~ ("," | guard(":"))) ^^ {
      case lp1 ~ lp2 => lp1 ++ lp2
    }
  lazy val lambdaStarEtc: PackratParser[List[_]] = (
    "*" ~> (id <~ lambdaSep) ~ rep(lambdaParamMaybeDefault) ^^ {
      case pos ~ l => ArbPosParam(pos) :: l
    } | ("*" ~ ",") ~> rep1(id ~ opt(default) <~ lambdaSep) ^^ {
      case lx => lx.map {
        case x ~ opt => KeyParam(x, opt)
      }
    }) ~ opt(lambdaKwds) ^^ {
      case lp ~ opt => opt.map(lp :+ _) getOrElse lp
    } | lambdaKwds ^^ { List(_) }
  lazy val lambdaKwds: PackratParser[_] = "**" ~> id <~ lambdaSep ^^ ArbKeyParam
  lazy val lambdaParamNoDefault: PackratParser[_] = id <~ lambdaSep ^^ { PosParam(_, None) }
  lazy val lambdaParamWithDefault: PackratParser[_] = id ~ default <~ lambdaSep ^^ {
    case x ~ e => PosParam(x, Some(e))
  }
  lazy val lambdaParamMaybeDefault: PackratParser[_] = id ~ opt(default) <~ lambdaSep ^^ {
    case x ~ opt => PosParam(x, opt)
  }
  lazy val lambdaSep = "," | guard(":")
  // lazy val lambdaParam: PackratParser[AId] = id

  // Expressions : production rules
  lazy val disjunction: PackratParser[Expr] = conjunction ~ rep1("or" ~> conjunction) ^^ {
    case e ~ el => el.foldLeft(e)( (sum, elem) => BoolExpr(OOr, sum, elem) )
  } | conjunction
  lazy val conjunction: PackratParser[Expr] = inversion ~ rep1("and" ~> inversion) ^^ {
    case e ~ el => el.foldLeft(e)( (sum, elem) => BoolExpr(OAnd, sum, elem) )
  } | inversion
  lazy val inversion: PackratParser[Expr] = "not" ~> inversion ^^ { UnaryExpr(UNot, _) } |
  comparison
  lazy val comparison: PackratParser[Expr] = bitOr ~ rep1(compareOpBitOrPair) ^^ {
    case be ~ lp => CompExpr(be, lp)
  } | bitOr
  lazy val compareOpBitOrPair: PackratParser[(CompOp, Expr)] = cop ~ bitOr ^^ {
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
    "await" ~> primary ^^ AwaitExpr | primary
  lazy val primary: PackratParser[Expr] =
    // //invalidPrimary |
    primary ~ ("." ~> id) ^^ { case e ~ i => Attribute(e, EName(i)) } |
// TODO: need arg model
//    primary ~ genexp ^^ {
//      case f ~ g => Call(f, List(NormalArg(g)))
//    } | primary ~ ("(" ~> opt(arguments) <~ ")") ^^ {
//      case f ~ opt => Call(f, opt.getOrElse(Nil))
//    } |
    primary ~ ("[" ~> slices <~ "]") ^^ {
      case p ~ s => Subscript(p, s)
    } |
    atom
  // Note that spec returns tuple of Expr
  // We change it to List of Epxr for the consistency of parsing beautified AST
  lazy val slices: PackratParser[Expr] = slice <~ not(",") |
    rep1sep(slice, ",") <~ opt(",") ^^ TupleExpr
  lazy val slice: PackratParser[Expr] =
    opt(expression) ~ (":" ~> opt(expression)) ~ opt(":" ~> opt(expression)) ^^ {
      case o1 ~ o2 ~ opt => Slice(o1, o2, opt.flatten)
    } | namedExpr
  
  // atoms : literal-like production
  lazy val atom: PackratParser[Expr] =
    id ^^ EName |
    "True" ^^^ EConst(BooleanLiteral(true)) |
    "False" ^^^ EConst(BooleanLiteral(false)) |
    "None" ^^^ EConst(NoneLiteral) |
    strings |
    number |
    (tuple | group | genexp) |
    (list | listcomp) // |
    // TODO: Dict with double starred
    // (dict | set | dictcomp | setcomp)

  // TODO make primitive parser for these
  lazy val strings: PackratParser[Expr] = stringLiteral
  
  // Displays (plain & comprehension)
  lazy val list: PackratParser[Expr] = "[" ~> opt(starNamedExprs) <~ "]" ^^ (l =>
    ListExpr(l.getOrElse(Nil))
  )
  lazy val listcomp: PackratParser[Expr] = "[" ~> (namedExpr ~ forIfClauses) <~ "]" ^^ {
    case e ~ complist => ListComp(e, complist)
  }  
  lazy val tuple: PackratParser[Expr] = "(" ~> opt(starNamedExpr ~ ("," ~>
    opt(starNamedExprs))) <~ ")" ^^ { listOpt =>
      val flattenOption = listOpt.map { case e ~ opt => e :: opt.getOrElse(Nil) }
      flattenOption.getOrElse(Nil)
    } ^^ TupleExpr
  lazy val group: PackratParser[Expr] = "(" ~> (yieldExpr | namedExpr) <~ ")" ^^ GroupExpr
  lazy val genexp: PackratParser[Expr] = "(" ~> ((assignExpr | expression <~ not(":=")) ~ forIfClauses) <~ ")" ^^ {
    case e ~ cel => GenComp(e, cel)
  }
  lazy val set: PackratParser[Expr] = "{" ~> starNamedExprs <~ "}" ^^ SetExpr
  lazy val setcomp: PackratParser[Expr] = "{" ~> namedExpr ~ forIfClauses <~ "}" ^^ {
    case e ~ complist => SetComp(e, complist)
  } 

  // TODO: Dict with double starred
//  lazy val dict: PackratParser[Expr] =  ("{" ~> opt(doubleStarredKvPairs) <~ "}" ^^  {
//    x => DictExpr(x.getOrElse(Nil))
//  }) | "{" ~> invalidDoubleStarredKvPairs <~ "}" 
//  lazy val dictcomp: PackratParser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ {
//    case kv ~ complist => DictCompExpr(kv, complist) 
//  }
//  lazy val doubleStarredKvPairs: PackratParser[List[DictItem]] = rep1sep(doubleStarredKvPair, ",") <~ opt(",")
//  lazy val doubleStarredKvPair: PackratParser[DictItem] =
//    "**" ~> bitOr ^^ DStarExpr ^^ DStarItem | kvPair
//  lazy val kvPair: PackratParser[DictItem] = expression ~ (":" ~> expression) ^^ {
//    case e1 ~ e2 => KvPair(e1, e2)
//  }

  // Comprehensions
  // Note. forIfClause same with comp_for
  lazy val forIfClauses: PackratParser[List[Comprehension]] = rep1(forIfClause)
  lazy val forIfClause: PackratParser[Comprehension] = (opt("async") <~ "for") ~
    (starTargets <~ "in") ~ disjunction ~ rep("if" ~> disjunction) ^^ {
      case None ~ target ~ in ~ conds => Comprehension(target, in, conds)
      case async ~ target ~ in ~ conds =>
        Comprehension(target, in, conds, async = true)
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
  lazy val starTarget: PackratParser[Expr] = ("*" ~ not("*")) ~> starTarget ^^ Starred |
    targetWithStarAtom
  lazy val targetWithStarAtom: PackratParser[Expr] = tPrimary ~ ("." ~> id <~ not(tLookahead)) ^^ {
    case prim ~ x => Attribute(prim, EName(x))
  } | tPrimary ~ ("[" ~> slices <~ "]" ~ not(tLookahead)) ^^ {
    case prim ~ s => Subscript(prim, s)
  } | starAtom
  lazy val starAtom: PackratParser[Expr] = id ^^ EName|
    "(" ~> targetWithStarAtom <~ ")" ^^ GroupExpr |
    "(" ~> opt(starTargetsTupleSeq) <~ ")" ^^ { e => TupleExpr(e.getOrElse(Nil)) } |
    "[" ~> opt(starTargetsListSeq) <~ "]" ^^ { e => ListExpr(e.getOrElse(Nil)) }
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
    tPrimary ~ ("." ~> id <~ guard(tLookahead)) ^^ {
      case prim ~ x => Attribute(prim, EName(x))
    } | tPrimary ~ ("[" ~> slices <~ "]" ~ guard(tLookahead)) ^^ {
      case prim ~ s => Subscript(prim, s)
      // TODO: need arg model
//    } | tPrimary ~ genexp <~ guard(tLookahead) ^^ {
//      case prim ~ gen => Call(prim, List(NormalArg(gen)))
//    } | tPrimary ~ ("(" ~> opt(arguments) <~ ")" ~ guard(tLookahead)) ^^ {
//      case prim ~ opt => Call(prim, opt.getOrElse(Nil))
    } | atom <~ guard(tLookahead)

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
  lazy val statementNewline: PackratParser[List[Stmt]] = 
    (compoundStmt <~ "\n") ^^ List(_) | simpleStmts | ("\n" ^^^ Nil) //| endmarker  //TODO ad rule for endmarker  
  
  lazy val simpleStmtsOne: PackratParser[List[Stmt]] = 
    (simpleStmt <~ (not(";") ~ "\n")) ^^ List(_)
  lazy val simpleStmts: PackratParser[List[Stmt]] = 
    simpleStmtsOne | (rep1sep(simpleStmt, ";") <~ (opt(";") ~ "\n"))

  lazy val simpleStmt: PackratParser[Stmt] =
    assignment |
    (starExprs ^^ ExprStmt) | 
    returnStmt | 
    importStmt | 
    raiseStmt | 
    ("pass" ^^^ PassStmt ) |
    delStmt | 
    yieldStmt | 
    assertStmt | 
    ("break" ^^^ BreakStmt) | 
    ("continue" ^^^  ContinueStmt) | 
    globalStmt | 
    nonlocalStmt |

  lazy val compoundStmt: PackratParser[Stmt] =
    funcDef | 
    ifStmt | 
    classDef | 
    withStmt | 
    forStmt | 
    tryStmt | 
    whileStmt | 
    matchStmt

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

  //////////////////////////////////////
  // Import stmt
  //////////////////////////////////////
  lazy val importStmt: PackratParser[Stmt] = (importName | importFrom)
  lazy val importName: PackratParser[Stmt] = ("import" ~ dottedAsNames) ^^ ImportStmt
  // TODO `.` and `...` related to relative import, need appropriate modeling
  lazy val importFrom: PackratParser[Stmt] = ( 
    (("from" ~> rep("." | "...")) ~ dottedName ~ ("import" ~> importFromTargets) ^^ {
      case _ ~ x ~ tl => ImportFromStmt(Some(x), tl)
    }) |
    (("from" ~> rep1("." | "...")) ~ ("import" ~> importFromTargets) ^^ {
      case _ ~ tl => ImportFromStmt(None, tl) // TODO what ... means?
    })
  ) 
  lazy val importFromTargets: PackratParser[List[Alias]] = (
    "(" ~ importFromAsNames ~ opt(",") ~ ")"
    | importFromAsNames ~ not(",")
    | ("*" ^^ ???)
  ) ^^ ???
  lazy val importFromAsNames: PackratParser[List[Alias]] =
    rep1sep(importFromAsName, ",")
  lazy val importFromAsName: PackratParser[Alias] = 
    (id ~ opt("as" ~ id)) ^^ { case x ~ opt => Alias(x, opt) }
  lazy val dottedAsNames: PackratParser[List[Alias]] = 
    rep1sep(dottedAsName, ",")
  lazy val dottedAsName: PackratParser[Alias] = 
    (dottedName ~ opt("as" ~> id)) ^^ { case x ~ opt => Alias(x, opt) }
  // TODO need additional modeling for this, alias includes dotted id sequence
  lazy val dottedName: PackratParser[Id] = (
    (dottedName ~ "." ~ id) ^^ ??? 
    | id
  ) 

  //////////////////////////////////////
  // Conditional Stmt
  //////////////////////////////////////
  lazy val ifStmt: PackratParser[Stmt] = (
    ("if" ~> namedExpr) ~ (":" ~> block) ~ elifStmt ^^ {
      case c ~ b ~ e => IfStmt(c, b, List(e))
    } |
    ("if" ~> namedExpr) ~ (":" ~> block) ~ opt(elseBlock) ^^ {
      case c ~ b ~ Some(eb) => IfStmt(c, b, eb)
      case c ~ b ~ None => IfStmt(c, b, Nil)
    }
  )
  // produces IfStmt, but only matched at the end of other ifStmt
  lazy val elifStmt: PackratParser[Stmt] = (
    ("elif" ~> namedExpr) ~ (":" ~> block) ~ elifStmt ^^ {
      case c ~ b ~ e => IfStmt(c, b, List(e)) 
    } | 
    ("elif" ~> namedExpr) ~ (":" ~> block) ~ opt(elseBlock) ^^ {
      case c ~ b ~ Some(eb) => IfStmt(c, b, eb)
      case c ~ b ~ None => IfStmt(c, b, Nil)
    }
  )
  lazy val elseBlock: PackratParser[List[Stmt]] = (("else" ~ ":") ~> block)

  //////////////////////////////////////
  // Loop Stmt
  //////////////////////////////////////
  lazy val whileStmt: PackratParser[Stmt] = 
    ("while" ~> namedExpr) ~ (":" ~> block) ~ opt(elseBlock) ^^ {
      case c ~ b ~ Some(el) => WhileStmt(c, b, el)
      case c ~ b ~ None => WhileStmt(c, b, Nil)
    }
  lazy val forStmt: PackratParser[Stmt] =
    opt("async") ~ ("for" ~> starTargets) ~ ("in" ~> starExprs) ~ (":" ~> (opt(typeComment) ~ block)) ~ opt(elseBlock) ^^ {
      case Some(_) ~ t ~ i ~ ty ~ b ~ Some(eb) => AsyncForStmt(ty, t, i, b, eb) 
      case Some(_) ~ t ~ i ~ ty ~ b ~ None => AsyncForStmt(ty, t, i, b, Nil) 
      case None ~ t ~ i ~ ty ~ b ~ Some(eb) => ForStmt(ty, t, i, b, eb) 
      case None ~ t ~ i ~ ty ~ b ~ None => ForStmt(ty, t, i, b, Nil) 
    }

  //////////////////////////////////////
  // With stmt
  //////////////////////////////////////
  lazy val withStmt: PackratParser[Stmt] = (
    opt("async") ~ (("with" ~ "(") ~> rep1sep(withItem, ",") <~ (opt(",") ~ ")")) ~ (":" ~> block) ^^ {
      case Some(_) ~ il ~ b => AsyncWithStmt(None, il, b) 
      case None ~ il ~ b => WithStmt(None, il, b)
    } |
    opt("async") ~ (("with" ~> rep1sep(withItem, ",")) ~ (":" ~> (opt(typeComment) ~ block))) ^^ {
      case Some(_) ~ (il ~ (tyOpt ~ b)) => AsyncWithStmt(tyOpt, il, b)
      case None ~ (il ~ (tyOpt ~ b)) => WithStmt(tyOpt, il, b)
    }
  ) 
  lazy val withItem: PackratParser[WithItem] = (
    expression ~ ("as" ~> starTarget) <~ guard("," | ")" | ":") ^^ {
      case e ~ t => WithItem(e, Some(t))
    } 
    | expression ^^ { case e => WithItem(e, None) }
  )
  
  //////////////////////////////////////
  // Try-except stmt
  //////////////////////////////////////
  lazy val tryStmt: PackratParser[Stmt] = (
    ("try" ~ ":") ~> block ~ finallyBlock ^^ {
      case b ~ fb => TryStmt(b, Nil, Nil, fb)
    } | 
    ("try" ~ ":") ~> block ~ rep1(exceptBlock) ~ opt(elseBlock) ~ opt(finallyBlock) ^^ {
      case b ~ hl ~ eopt ~ fopt => TryStmt(b, hl, eopt.getOrElse(Nil), fopt.getOrElse(Nil))
    }
  )
  lazy val exceptBlock: PackratParser[ExcHandler] = (
    ("except" ~> expression) ~ opt("as" ~> id) ~ (":" ~> block) ^^ {
      case e ~ iopt ~ b => ExcHandler(e, iopt, b)
    }
    | ("except" ~ ":") ~> block ^^ {
      case b => ExcHandler(???, None, b)
    }
  )
  lazy val finallyBlock: PackratParser[List[Stmt]] = ("finally" ~ ":") ~> block

  //////////////////////////////////////
  // Match stmt
  //////////////////////////////////////
  lazy val matchStmt: PackratParser[Stmt] =
    "match" ~> subjectExpr ~ (":" ~ "\n" ~ indent ~> rep1(caseBlock) <~ dedent) ^^ {
      case e ~ cl => MatchStmt(e, cl)
    }
  lazy val subjectExpr: PackratParser[Expr] =
    ((starNamedExpr <~ ",") ~ opt(starNamedExprs)) ^^ {
      case e ~ opt => TupleExpr(e :: opt.getOrElse(Nil))
    } | namedExpr

  lazy val caseBlock: PackratParser[MatchCase] =
    "case" ~> patterns ~ opt("if" ~> namedExpr) ~ (":" ~> block) ^^ {
      case p ~ opt ~ b => MatchCase(p, opt, b)
    }

  // Patterns
  lazy val patterns: PackratParser[Pattern] = openSeqPattern ^^ MatchSeq| pattern
  lazy val pattern: PackratParser[Pattern] = asPat | orPat
  lazy val asPat: PackratParser[Pattern] = orPat ~ ("as" ~> patCaptureTarget) ^^ {
    case or ~ x => MatchAs(or, x)
  }
  lazy val orPat: PackratParser[Pattern] = rep1sep(closedPat, "|") ^^ {
    case head :: Nil => head
    case lp => MatchOr(lp)
  }
  lazy val closedPat: PackratParser[Pattern] = literalPat | capturePat | wildcardPat |
    valuePat | groupPat | seqPat | mapPat | classPat
  lazy val literalPat: PackratParser[Pattern] =
    (signedNum <~ not("+" | "-") | complexNum | strings) ^^ MatchValue |
    ("None" | "True" | "False") ^^ MatchSingleton
  lazy val literalExpr: PackratParser[Expr] =
    signedNum <~ not("+" | "-") | complexNum | strings |
    ("None" | "True" | "False") ^^ EConst
  lazy val complexNum: PackratParser[Expr] = signedRealNum ~ sumop ~ imagNum ^^ {
    case r ~ op ~ j => BinaryExpr(op, r, j)
  }
  lazy val signedNum: PackratParser[Expr] =
    number | "-" ~> number ^^ { UnaryExpr(UMinus, _) }
  lazy val realNum = number
  lazy val imagNum = number
  lazy val capturePat: PackratParser[Pattern] = patCaptureTarget ^^ ??? // matchas
  lazy val patCaptureTarget: PackratParser[Id] = not("_") ~> id <~ not("." | "(" | "=")
  lazy val wildcardPat: PackratParse[Pattern] = "_" ^^ ???
  lazy val valuePat: PackratParse[Pattern] = attr ~ not("." | "(" | "=") ^^ MatchValue
  lazy val attr: PackratParse[Expr] = nameOrAttr ~ ("." ~> id) ^^ {
    case e ~ x => Attribute(e, x)
  }
  lazy val nameOrAttr: PackratParse[Expr] = attr | id
  lazy val groupPat: PackratParse[Pattern] = "(" ~> pattern <~ ")" ^^ MatchGroup
  lazy val seqPat: PackratParse[Pattern] =
    ("[" ~> opt(maybeSeqPat) <~ "]" | "(" ~> opt(openSeqPat) <~ ")") MatchSeq
  lazy val openSeqPat: PackratParse[List[Pattern]] =
    maybeStarPat ~ ("," ~> maybeSeqPat) ^^ {
      case p ~ opt => MatchSeq(p :: opt.getOrElse(Nil))
    }
  lazy val maybeSeqPat: PackratParse[List[Pattern]] =
    rep1sep(maybeStarPattern, ",") <~ opt(",")
  lazy val maybeStarPat: PackratParse[Patter] = starPat | pattern
  lazy val starPat: PackratParse[Patter] = "*" ~> patCaptureTarget ^^ (
    x => MatchStar(Some(x))
  ) | "*" ~> wildcardPat ^^^ MatchStar(None)
//TODO: Add double star unpacking
  lazy val mapPat: PackratParse[Pattern] =
    "{" ~ "}" ^^ _ => MatchMapping(Nil, None) |
    "{" ~> doubleStarPat <~ opt(",") ~ "}" ^^ ??? |
    "{" ~> itemsPat ~ ("," ~> doubleStarPat) <~ opt(",") ~ "}" ^^ ??? |
    "{" ~> itemsPat <~ opt(",") ~ "}" ^^ ???
  lazy val itemsPat: PackratParse[List[Pattern]] = rep1sep(kvPat, ",")
  // TODO: model the pair
  lazy val kvPat: PackratParse[Patter] = ???
  lazy val doubleStarPat: PackratParse[Id] = "**" ~> patCaptureTarget
  // TODO: model the class pattern
  lazy val classPat: PackratParse[Pattern] = ???
  lazy val posPats: PackratParse[List[Pattern]] = rep1sep(pattern, ",")
  lazy val keyPats: PackratParse[List[Pattern]] = rep1sep(keyPat, ",")
  // TODO: model the pair
  lazy val keyPat: PackratParse[Pattern] = id ~ ("=" ~> pattern) ^^ ???


  //////////////////////////////////////
  // Return, Raise stmt
  //////////////////////////////////////
  lazy val returnStmt: PackratParser[Stmt] = 
    ("return" ~> opt(starExprs)) ^^ ReturnStmt
  lazy val raiseStmt: PackratParser[Stmt] = (
    ("raise" ~> expression) ~ opt("from" ~> expression) ^^ {
      case e ~ fopt =>  RaiseStmt(Some(e), fopt)
    } | 
    ("raise") ^^^ RaiseStmt(None, None)
  )

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
  lazy val paramList: PackratParser[List[_]] =
    defparam ~ ("," ~> defparam).* ~ (("," ~ "/") ~> opt("," ~> opt(paramListNoPosonly))) ^^ {
      // no posonly
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes posonly
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListNoPosonly

  lazy val paramListNoPosonly: PackratParser[List[_]] =
    defparam ~ ("," ~> defparam).* ~ opt("," ~> opt(paramListStarargs)) ^^ {
      // no starparams
      case p ~ pl ~ None => p +: pl
      case p ~ pl ~ Some(None) => p +: pl
      // yes starparams
      case p ~ pl ~ Some(Some(sl)) => (p +: pl) ++ sl
    } |
    paramListStarargs

  // parser for parameters appear after star or double-star
  lazy val paramListStarargs: PackratParser[List[_]] =
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
  lazy val oneStarParam: PackratParser[Option[_]] = ("*" ~> opt(param)) ^^ {
    case Some(i) => Some(ArbPosParam(i))
    case None => None
  }
  // parser for arbitrary keyword parameter
  lazy val doubleStarParam: PackratParser[_] = "**" ~> param <~ opt(",") ^^ {
    case i => ArbKeyParam(i) 
  }
  // parser for normal parameter
  lazy val defparam: PackratParser[_] = param ~ ("=" ~> opt(expression)) ^^ {
    case i ~ oe => PosParam(i, oe)
  }
  // parser for parameter id
  lazy val param: PackratParser[Id] = id // TODO add optional type expr `: expr`
  lazy val default: PackratParser[Expr] = "=" ~> expression

  // class def
  lazy val classDef: PackratParser[Stmt] = (
    decorators ~ classDefRaw
    |  classDefRaw
  ) ^^ ???
  lazy val classDefRaw: PackratParser[Stmt] =
    ("class" ~ id ~ opt("(" ~ args ~ ")") ~ ":" ~ block) ^^ ???

  /////////////////////////////////
  // Block : List of Stmt
  /////////////////////////////////
  lazy val block: PackratParser[List[Stmt]] = (
    ("\n" ~ indent) ~> statements <~ dedent
    | simpleStmts
  )

  /////////////////////////////////
  // Invalid productions
  ////////////////////////////////
  // invalid productions accepts come ill-formed subexpr and raise syntax erorr early
  def error(msg: String): Parser[Nothing] = Parser(in => firstMap(in, _ => Error(msg, in)))

  lazy val invalidPrimary: Parser[Nothing] = (primary ~ "{").into(_ => error("invalid syntax"))
  // TODO: Dict with double starred
//  lazy val invalidDoubleStarredKvPairs: Parser[Nothing] =
//    ( repsep(doubleStarredKvPair, ",") ~ "," ~ invalidKvPair
//      | expression ~ ":" ~ "*" ~ bitOr
//      | expression ~ ":" ~ guard("}"|",") 
//    ).into(_ => error("invalid syntax")) //TODO : appropriate errormessage
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
    "Listcomp" -> listcomp,
    "Tuple" -> tuple,
    "Set" -> set,
    "Setcomp" -> setcomp,
//    "Dict" -> dict,
//    "Dictcomp" -> dictcomp,
    "ForIfClause" -> forIfClause,
    "StarTargets" -> starTargets,
    "StarTarget" -> starTarget,
    "TargetWithStarAtom" -> targetWithStarAtom,
    "StarAtom" -> starAtom,
    // TODO: consider lookahead
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
//    "Lambdef" -> lambdef,
    // Expression
    "Expression" -> expression,
    "Expressions" -> expressions,
    "NamedExpr" -> namedExpr,
    "AssignExpr" -> assignExpr,
    "StarNamedExpr" -> starNamedExpr,
    "StarExpr" -> starExpr,
    "StarExprs" -> starExprs,
    "YieldExpr" -> yieldExpr,
//    // Statement
//    "PassStmt" -> passStmt,
//    "BreakStmt" -> breakStmt,
//    "ContinueStmt" -> continueStmt,
//    // "Assignment" -> assignment,
//    "GlobalStmt" -> globalStmt,
//    "NonlocalStmt" -> nonlocalStmt,
//    "YieldStmt" -> yieldStmt,
//    "AssertStmt" -> assertStmt,
//    // "DelStmt" -> delStmt,
  )
}

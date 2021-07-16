package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  def apply(tokens: Seq[Token]): List[Stmt] = statements(
    new PackratReader(TokenListParser.TokenReader(tokens))
  ).get
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
 
  
  //////////////////////////////////////////////////////////////////
  // Base case parsers
  //////////////////////////////////////////////////////////////////
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
  lazy val stringLiteral: PackratParser[Const] = log(Parser(in => firstMap(in, _ match {
    case StrToken(s) => Success(StringLiteral(s), in.rest)
    case t => Failure(s"", in)
  })))("stringLiteral")

// TODO: Add bytesLiteral model
//  lazy val bytesLiteral: PackratParser[Expr] = log(Parser(in => firstMap(in, _ match {
//    case BytesLiteral(b) => Success(ABytesLiteral(b), in.rest)
//    case t => Failure(s"", in)
//  })))("bytesLiteral")

  lazy val intLiteral: PackratParser[Const] = log(Parser(in => firstMap(in, _ match {
    case IntToken(i) => Success(IntLiteral(i.toInt), in.rest)
    case t => Failure(s"", in)
  })))("intLiteral")

  lazy val floatLiteral: PackratParser[Const] = log(Parser(in => firstMap(in, _ match {
    case FloatToken(f) => Success(FloatLiteral(f.toDouble), in.rest)
    case t => Failure(s"", in) 
  })))("floatLiteral")

  lazy val imagLiteral: PackratParser[Const] = log(Parser(in => firstMap(in, _ match {
    case ImagToken(i) => Success(ComplexLiteral(i.toDouble), in.rest)
    case t => Failure(s"", in)
  })))("imagLiteral")

  lazy val boolLiteral: PackratParser[Const] =
    "True" ^^^ BooleanLiteral(true) |
    "False" ^^^ BooleanLiteral(false)
  lazy val noneLiteral: PackratParser[Const] = "None" ^^^ NoneLiteral

 // some token parsers
  lazy val indent: PackratParser[Unit] = log(Parser(in => firstMap(in, _ match {
    case IndentToken => Success((), in.rest)
    case _ => Failure(s"", in)
  })))("indent")
  lazy val dedent: PackratParser[Unit] = log(Parser(in => firstMap(in, _ match {
    case DedentToken => Success((), in.rest)
    case _ => Failure(s"", in)
  })))("dedent")
  lazy val nl: PackratParser[String] = log(Parser(in => firstMap(in, _ match {
    case NewlineToken(None) => Success("", in.rest)
    case NewlineToken(Some(s)) => Success(s, in.rest)
    case _ => Failure(s"", in)
  })))("nl")
  lazy val comment: PackratParser[String] = log(Parser(in => firstMap(in, _ match {
    case CommentToken(s) => Success(s, in.rest)
    case _ => Failure(s"", in)
  })))("comment")
  
   // type comment starts with `# type: ` (including whitespaces)
   // corresponds to `TYPE_COMMENT` in spec
  lazy val typeComment: PackratParser[String] = log(comment)("typeComment")
  lazy val funcTypeComment: PackratParser[String] = (
    "\n" ~> typeComment <~ guard(nl ~ indent) |
    typeComment
  )

  lazy val number: PackratParser[Expr] =
    (intLiteral | floatLiteral | imagLiteral) ^^ EConst
  lazy val bool: PackratParser[Expr] = boolLiteral ^^ EConst
  lazy val none: PackratParser[Expr] = noneLiteral ^^ EConst

  private def splitText(s: String): List[String] =
    "([a-zA-Z0-9_]+|\\S)".r.findAllIn(s).toList

  implicit def text(str: String): PackratParser[String] = {
    Parser(in => {
      firstMap(in, t => t match {
          case NewlineToken(_) if str == "\n" => Success(s"\n", in.rest) 
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
    } | disjunction | lambdef
  )("expression")

  ///////////////////////////////////////////
  // Lambda expressions
  ///////////////////////////////////////////
  lazy val lambdef: PackratParser[Expr] = log(("lambda" ~> opt(lambdaParams) <~ ":") ~ expression ^^ {
    case Some(a) ~ e => LambdaExpr(a, e)
    case None ~ e => LambdaExpr(Args(), e)
  })("lambdef")

  // Basically same with production `parameters`
  lazy val lambdaParams: PackratParser[Args] = lambdaSlashNoDefault ~
    rep(lambdaParamNoDefault) ~ rep(lambdaParamWithDefault) ~ opt(lambdaStarEtc) ^^ {
      case snl ~ pnl ~ pwl ~ kopt => {
        val posonly: List[(Arg, Option[Expr])] = snl.map(a => (a, None)) 
        val midl: List[(Arg, Option[Expr])] = pnl.map(a =>  (a, None))
        kopt match {
          case Some(args) => args.copy(posOnlys =  posonly, normArgs = midl)
          case None => Args(posonly, midl)
        }
      }
    } | lambdaSlashWithDefault ~ rep(lambdaParamWithDefault) ~ opt(lambdaStarEtc) ^^ {
      case (snl, swl) ~ pwl ~ kopt => {
        val posNoDefault: List[(Arg, Option[Expr])]  = snl.map(a => (a, None))
        val posonly = posNoDefault ++ swl
        kopt match {
          case Some(args) => args.copy(posOnlys = posonly, normArgs= pwl)
          case None => Args(posonly, pwl)
        }
      }
    } | rep1(lambdaParamNoDefault) ~ rep(lambdaParamWithDefault) ~ opt(lambdaStarEtc) ^^ {
      case pnl ~ pwl ~ kopt => { 
        val normNd: List[(Arg, Option[Expr])] = pnl.map(a => (a, None))
        val normargs = normNd ++ pwl
        kopt match {
          case Some(args) => args.copy(normArgs = normargs)
          case None => Args(normArgs = normargs)
        }
      }
    } | rep1(lambdaParamWithDefault) ~ opt(lambdaStarEtc) ^^ {
      case pwl ~ kopt => {
        kopt match {
          case Some(args) => args.copy(normArgs = pwl)
          case None => Args(normArgs = pwl)
        }
      }
    } | lambdaStarEtc
  // pos only
  lazy val lambdaSlashNoDefault: PackratParser[List[Arg]] =
    rep1(lambdaParamNoDefault) <~ "/" ~ lambdaSep
  // pos only
  lazy val lambdaSlashWithDefault: PackratParser[(List[Arg], List[(Arg, Option[Expr])])] =
    rep(lambdaParamNoDefault) ~ rep1(lambdaParamWithDefault) <~ "/" ~ lambdaSep ^^ {
      case pnl ~ pwl => (pnl, pwl)
    }
  // vararg, keyword only arg, kwargs can appear
  lazy val lambdaStarEtc: PackratParser[Args] = ( 
    ("*" ~> lambdaParamNoDefault) ~ rep(lambdaParamMaybeDefault) ~ opt(lambdaKwds) ^^ {
      case a ~ pwl ~ kopt => Args(varArg = Some(a), keyOnlys = pwl, kwArg = kopt)
    } | "*" ~ "," ~> rep1(lambdaParamMaybeDefault) ~ opt(lambdaKwds) ^^ {
      case pml ~ kopt => Args(keyOnlys = pml, kwArg = kopt)
    } | lambdaKwds ^^ { case a => Args(kwArg = Some(a)) }
  )
  lazy val lambdaKwds: PackratParser[Arg] = "**" ~> lambdaParamNoDefault
  lazy val lambdaParamNoDefault: PackratParser[Arg] =
    lambdaParam <~ lambdaSep ^^ { Arg(_) }
  lazy val lambdaParamWithDefault: PackratParser[(Arg, Option[Expr])] =
    lambdaParam ~ default <~ lambdaSep ^^ {
      case x ~ e => (Arg(x), Some(e))
    }
  lazy val lambdaParamMaybeDefault: PackratParser[(Arg, Option[Expr])] = 
    lambdaParam ~ opt(default) <~ lambdaSep ^^ {
      case x ~ dopt => (Arg(x), dopt)
    }
  lazy val lambdaParam: PackratParser[Id] = id
  lazy val lambdaSep = "," | guard(":")

  ///////////////////////////////////////////
  // Expressions : production rules
  ///////////////////////////////////////////
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
    primary ~ ("." ~> id) ^^ { case e ~ i => Attribute(e, i) } |
    primary ~ genexp ^^ {
      case f ~ g => Call(f, List(g))
    } | primary ~ ("(" ~> opt(arguments) <~ ")") ^^ {
      case f ~ Some((le, lk)) => Call(f, le, lk)
      case f ~ None => Call(f)
    } |
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
    bool |
    none |
    strings |
    number |
    (tuple | group | genexp) |
    (list | listcomp) |
    (dict | set | dictcomp | setcomp) |
    "..." ^^^ EConst(Ellipsis)

  // TODO make primitive parser for these
  lazy val strings: PackratParser[Expr] = stringLiteral ^^ EConst
  
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

  lazy val dict: PackratParser[Expr] = "{" ~> opt(doubleStarredKvPairs) <~ "}" ^^  {
    case list =>
      val (lp, le) = list.map(_.foldRight((List[(Expr, Expr)](), List[Expr]())){
        case ((Some(p), None), (lp, le)) => (p :: lp, le)
        case ((None, Some(e)), (lp, le)) => (lp, e :: le)
        case _ => ???
      }).getOrElse((Nil, Nil))
      DictExpr(lp, le)
   } // | "{" ~> invalidDoubleStarredKvPairs <~ "}"
  lazy val dictcomp: PackratParser[Expr] = "{" ~> (kvPair ~ forIfClauses) <~ "}" ^^ {
    case kv ~ lcomp => DictComp(kv, lcomp)
  }
  lazy val doubleStarredKvPairs: PackratParser[List[(Option[(Expr, Expr)], Option[Expr])]] =
    rep1sep(doubleStarredKvPair, ",") <~ opt(",")
  lazy val doubleStarredKvPair: PackratParser[(Option[(Expr, Expr)], Option[Expr])] =
    "**" ~> bitOr ^^ (dstar => (None, Some(DoubleStarred(dstar)))) |
    kvPair ^^ (kvpair => (Some(kvpair), None))
  lazy val kvPair: PackratParser[(Expr, Expr)] = expression ~ (":" ~> expression) ^^ {
    case e1 ~ e2 => (e1, e2)
  }

  // Comprehensions
  // Note. forIfClause same with comp_for
  lazy val forIfClauses: PackratParser[List[Comprehension]] = rep1(forIfClause)
  lazy val forIfClause: PackratParser[Comprehension] = (opt("async") <~ "for") ~
    (starTargets <~ "in") ~ disjunction ~ rep("if" ~> disjunction) ^^ {
      case None ~ target ~ in ~ conds => Compre(target, in, conds)
      case async ~ target ~ in ~ conds => AsyncCompre(target, in, conds)
    }
  lazy val yieldExpr: PackratParser[Expr] = 
    ("yield" ~ "from") ~> expression ^^ YieldFromExpr |
    "yield" ~> opt(starExprs) ^^ YieldExpr
    // TODO: scala operator precedance problem here with | 

  //////////////////////////////////////////////////////////////////
  // Arguments
  // for one arg, Expr or Kwarg(id?, expr) can be returned.
  // unpacking list (ie. *arg_list) : Starred in List[Expr]
  // unpacking dict (ie. **arg_dict) : (None, DoubleStarred) is List[Kwarg]
  /////////////////////////////////////////////////////////////////
  lazy val arguments: PackratParser[(List[Expr], List[Kwarg])] = args <~ (opt(",") ~ guard(")"))
  lazy val args: PackratParser[(List[Expr], List[Kwarg])] = ( 
    (rep1sep(starredExpr | (assignExpr | expression <~ not(":=")) <~ not("="), ",")) ~ opt("," ~> kwargs) ^^ {
      case el ~ None => (el, Nil)
      case el ~ Some((exprList, kwList)) => (el ++ exprList,  kwList)   
    } | 
    kwargs
  )
  // getting all Kwarg
  lazy val kwargs: PackratParser[(List[Expr], List[Kwarg])] = (
    rep1sep(kwargOrStarred, ",") ~ rep("," ~> kwargOrDoubleStarred) ^^ { 
      case eitherList ~ kwargList => {
        def matcher(p: (List[Expr], List[Kwarg]), e: Either[Expr, Kwarg]): (List[Expr], List[Kwarg]) = p match {
          case (el, kl) => e match {
            case Left(e) => (el :+ e, kl)
            case Right(k) => (el, kl :+ k)
          }
        }
        val (el, kl) = eitherList.foldLeft(List[Expr](), List[Kwarg]())(matcher)
        (el, kl ++ kwargList)
      }
    } | 
    rep1sep(kwargOrStarred, ",") ^^ {
      case eitherList => { // List[Either[Expr, Kwarg]] -> (List[Expr], List[Kwarg])
        def matcher(p: (List[Expr], List[Kwarg]), e: Either[Expr, Kwarg]): (List[Expr], List[Kwarg]) = p match {
          case (el, kl) => e match {
            case Left(e) => (el :+ e, kl)
            case Right(k) => (el, kl :+ k)
          }
        }
        val (el, kl) = eitherList.foldLeft(List[Expr](), List[Kwarg]())(matcher)
        (el, kl)
      }
    } |
    rep1sep(kwargOrDoubleStarred, ",")  ^^ {
      case kl => (Nil, kl)
    }
  )
  // vararg case : must be Starred
  lazy val starredExpr: PackratParser[Starred] = ("*" ~> expression) ^^ { case e => Starred(e) }
  // keyarg or vararg case
  lazy val kwargOrStarred: PackratParser[Either[Expr, Kwarg]] = (
    id ~ ("=" ~> expression) ^^ { case i ~ e => Right(Kwarg(Some(i), e)) } | 
    starredExpr ^^ { case e => Left(e) }
  )
  // keyarg or kwargs case
  lazy val kwargOrDoubleStarred: PackratParser[Kwarg] = ( 
    id ~ ("=" ~> expression) ^^ { case i ~ e => Kwarg(Some(i), e) } | 
    ("**" ~> expression) ^^ { 
      case e => Kwarg(None, DoubleStarred(e))
    }
  )

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
    case prim ~ x => Attribute(prim, x)
  } | tPrimary ~ ("[" ~> slices <~ "]" ~ not(tLookahead)) ^^ {
    case prim ~ s => Subscript(prim, s)
  } | starAtom
  lazy val starAtom: PackratParser[Expr] = id ^^ EName|
    "(" ~> targetWithStarAtom <~ ")" ^^ GroupExpr |
    "(" ~> opt(starTargetsTupleSeq) <~ ")" ^^ { e => TupleExpr(e.getOrElse(Nil)) } |
    "[" ~> opt(starTargetsListSeq) <~ "]" ^^ { e => ListExpr(e.getOrElse(Nil)) }
  lazy val singleTarget: PackratParser[Expr] = (
    singleSubscriptAttrTarget | 
    id ^^ EName | 
    ("(" ~> singleTarget <~ ")")
  )
  lazy val singleSubscriptAttrTarget: PackratParser[Expr] = (
    tPrimary ~ ("." ~> id) <~ not(tLookahead) ^^ {
      case prim ~ x => Attribute(prim, x)
    } | 
    tPrimary ~ ("[" ~> slices <~ "]") <~ not(tLookahead) ^^ {
      case prim ~ s => Subscript(prim, s)
    } | 
    starAtom
  )

  // nested lists / tuples of targts can  appear.
  // `delTargets` flatten them into one-level list
  lazy val delTargets: PackratParser[List[Expr]] = 
    (rep1sep(delTarget, ",") <~ opt(",")) ^^ {
      case l => l.flatten
    }
  lazy val delTarget: PackratParser[List[Expr]] = (
    tPrimary ~ ("." ~> id) <~ not(tLookahead) ^^ {
      case prim ~ x => List(Attribute(prim, x))
    }
    | tPrimary ~ ("[" ~> slices <~ "]") <~ not(tLookahead) ^^ {
      case prim ~ s => List(Subscript(prim, s)) 
    }
    | deltAtom
  )
  lazy val deltAtom: PackratParser[List[Expr]] = (
    id ^^ { case x => List(EName(x)) } | 
    "(" ~> delTarget <~ ")" | 
    "(" ~> opt(delTargets) <~ ")" ^^ { case o => o.getOrElse(Nil) } | 
    "[" ~> opt(delTargets) <~ "]" ^^ { case o => o.getOrElse(Nil) }
  )

  lazy val tPrimary: PackratParser[Expr] = (
    tPrimary ~ ("." ~> id <~ guard(tLookahead)) ^^ {
      case prim ~ x => Attribute(prim, x)
    } | 
    tPrimary ~ ("[" ~> slices <~ "]" ~ guard(tLookahead)) ^^ {
      case prim ~ s => Subscript(prim, s)
    } | 
    tPrimary ~ genexp <~ guard(tLookahead) ^^ {
      case prim ~ g => Call(prim, List(g))
    } | 
    tPrimary ~ ("(" ~> opt(arguments) <~ ")" ~ guard(tLookahead)) ^^ {
      case prim ~ Some((le, lk)) => Call(prim, le, lk)
      case prim ~ None => Call(prim)
    } | 
    atom <~ guard(tLookahead)
  )
  lazy val tLookahead: PackratParser[String] = "(" | "[" | "."
  ////////////////////////////////////////////////////////////////////////////////
  // Top-level module
  ////////////////////////////////////////////////////////////////////////////////
  lazy val module: PackratParser[Module] = statements ^^ { case sl => Module(sl) }
  ////////////////////////////////////////////////////////////////////////////////
  // Statements
  //////////////////////////////////////////////////////////////////////////////
  lazy val statements: PackratParser[List[Stmt]] = rep1(stmtOrComments) ^^ {
    case ol => ol.flatten
  }
  lazy val stmtOrComments: PackratParser[Option[Stmt]] = (
    statement <~ (opt(comment) ~ opt("\n")) ^^ { case s => Some(s) } |
    opt(comment) ~ nl ^^ { case _ => None }
  )
  lazy val statement: PackratParser[Stmt] =
    compoundStmt | simpleStmts  
  
  lazy val simpleStmtsOne: PackratParser[Stmt] = (simpleStmt <~ (not(";") ~ "\n")) 
  lazy val simpleStmts: PackratParser[Stmt] = ( 
    simpleStmtsOne | 
    (rep1sep(simpleStmt, ";") <~ (opt(";") ~ "\n")) ^^ OnelineStmt
  )

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
    nonlocalStmt 

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
  lazy val assignment: PackratParser[Stmt] = (
    (id ~ (":" ~> expression) ~ opt("=" ~> annotatedRhs) ^^ {  
      case x ~ ty ~ rhs => AnnAssign(EName(x), ty, rhs)
    }) |
    ( (("(" ~> singleTarget <~ ")") | singleSubscriptAttrTarget) ~ (":" ~> expression) ~ opt("=" ~> annotatedRhs) ) ^^ { 
      case te ~ ty ~ rhs => AnnAssign(te, ty, rhs)
    } |
    (rep1(starTargets <~ "=") ~ (yieldExpr | starExprs) ~ (not("=") ~> opt(typeComment))) ^^ {
      case el ~ e ~ tyopt => AssignStmt(el, e, tyopt)
    } | 
    (singleTarget ~ augAssign ~ (yieldExpr | starExprs)) ^^ {
      case t ~ op ~ e => AugAssign(t, op, e)
    } 
  )
  lazy val augAssign: PackratParser[BinOp] = ( 
    ("+=" | "-=" | "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=") ^^ {
      case s => Op.getBinOp(s.dropRight(1)).getOrElse(???) // TODO raise appropriate error
    }
  )
 
  // some simple stmt
  lazy val globalStmt: PackratParser[Stmt] = ("global" ~> rep1sep(id, ",")) ^^ GlobalStmt 
  lazy val nonlocalStmt: PackratParser[Stmt] = ("nonlocal" ~> rep1sep(id, ",")) ^^ NonlocalStmt
  lazy val yieldStmt: PackratParser[Stmt] = yieldExpr ^^ ExprStmt
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
  lazy val importName: PackratParser[Stmt] = ("import" ~> dottedAsNames) ^^ ImportStmt
  // helper fn for relative import
  def relLevel(l: List[String]): Int = l.mkString("").length()
  lazy val importFrom: PackratParser[Stmt] = ( 
    (("from" ~> rep("." | "...")) ~ dottedName ~ ("import" ~> importFromTargets) ^^ {
      case rl ~ x ~ tl => ImportFromStmt(relLevel(rl), x, tl)
    }) |
    (("from" ~> rep1("." | "...")) ~ ("import" ~> importFromTargets) ^^ {
      case rl ~ tl => ImportFromStmt(relLevel(rl), Nil, tl) 
    })
  ) 
  lazy val importFromTargets: PackratParser[List[Alias]] = (
    ("(" ~> importFromAsNames <~ (opt(",") ~ ")"))
    | importFromAsNames <~ not(",")
    | ("*" ^^^ Nil) // Remark: Nil means import all `*`
  )
  lazy val importFromAsNames: PackratParser[List[Alias]] =
    rep1sep(importFromAsName, ",")
  lazy val importFromAsName: PackratParser[Alias] = 
    (id ~ opt("as" ~> id)) ^^ { case x ~ opt => Alias(List(x), opt) }
  lazy val dottedAsNames: PackratParser[List[Alias]] = 
    rep1sep(dottedAsName, ",")
  lazy val dottedAsName: PackratParser[Alias] = 
    (dottedName ~ opt("as" ~> id)) ^^ { case il ~ opt => Alias(il, opt) }
  // TODO need additional modeling for this, alias includes dotted id sequence
  lazy val dottedName: PackratParser[List[Id]] = (
    (dottedName ~ ("." ~> id)) ^^ { case il ~ x => il :+ x } 
    | id ^^ { case x => List(x) }
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
    (opt("async") ~ ("for" ~> starTargets)) ~ (("in" ~> starExprs) ~ (":" ~> (opt(typeComment) ~ block))) ~ opt(elseBlock) ^^ {
      case (Some(_) ~ t) ~ (i ~ (ty ~ b)) ~ Some(eb) => AsyncForStmt(ty, t, i, b, eb) 
      case (Some(_) ~ t) ~ (i ~ (ty ~ b)) ~ None => AsyncForStmt(ty, t, i, b, Nil) 
      case (None ~ t) ~ (i ~ (ty ~ b)) ~ Some(eb) => ForStmt(ty, t, i, b, eb) 
      case (None ~ t) ~ (i ~ (ty ~ b)) ~ None => ForStmt(ty, t, i, b, Nil) 
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
  lazy val exceptBlock: PackratParser[ExcHandler] =
    "except" ~> expression ~ opt("as" ~> id) ~ (":" ~> block) ^^ {
      case e ~ iopt ~ b => ExcHandler(Some(e), iopt, b)
    } | "except" ~ ":" ~> block ^^ { ExcHandler(None, None, _) }
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
  lazy val patterns: PackratParser[Pattern] = openSeqPat ^^ MatchSeq | pattern
  lazy val pattern: PackratParser[Pattern] = asPat | orPat
  lazy val asPat: PackratParser[Pattern] = orPat ~ ("as" ~> patCaptureTarget) ^^ {
    case or ~ x => MatchAs(Some(or), x)
  }
  lazy val orPat: PackratParser[Pattern] = rep1sep(closedPat, "|") ^^ {
    case head :: Nil => head
    case lp => MatchOr(lp)
  }
  lazy val closedPat: PackratParser[Pattern] = literalPat | capturePat | wildcardPat |
    valuePat | groupPat | seqPat | mapPat | classPat
  lazy val literalPat: PackratParser[Pattern] =
    (signedNum <~ not("+" | "-") | complexNum | strings) ^^ MatchValue |
    (noneLiteral | boolLiteral) ^^ MatchSingleton
  lazy val literalExpr: PackratParser[Expr] = signedNum <~ not("+" | "-") |
    complexNum | strings | none | bool
  lazy val complexNum: PackratParser[Expr] = signedRealNum ~ sumop ~ imagNum ^^ {
    case r ~ op ~ j => BinaryExpr(op, r, j)
  }
  lazy val signedNum: PackratParser[Expr] =
    number | "-" ~> number ^^ { UnaryExpr(UMinus, _) }
  lazy val signedRealNum: PackratParser[Expr] =
    realNum | "-" ~> number ^^ { UnaryExpr(UMinus, _) }
  lazy val realNum: PackratParser[Expr] = number
  lazy val imagNum: PackratParser[Expr] = number
  lazy val capturePat: PackratParser[Pattern] = patCaptureTarget ^^ { MatchAs(None, _) }
  lazy val patCaptureTarget: PackratParser[Id] = not("_") ~> id <~ not("." | "(" | "=")
  lazy val wildcardPat: PackratParser[Pattern] = "_" ^^^ MatchWildcard
  lazy val valuePat: PackratParser[Pattern] = attr <~ not("." | "(" | "=") ^^ MatchValue
  lazy val attr: PackratParser[Expr] = nameOrAttr ~ ("." ~> id) ^^ {
    case e ~ x => Attribute(e, x)
  }
  lazy val nameOrAttr: PackratParser[Expr] = attr | id ^^ EName
  lazy val groupPat: PackratParser[Pattern] = "(" ~> pattern <~ ")" ^^ MatchGroup
  lazy val seqPat: PackratParser[Pattern] =
    ("[" ~> opt(maybeSeqPat) <~ "]" | "(" ~> opt(openSeqPat) <~ ")") ^^ {
      case opt => MatchSeq(opt.getOrElse(Nil))
    }
  lazy val openSeqPat: PackratParser[List[Pattern]] =
    maybeStarPat ~ ("," ~> opt(maybeSeqPat)) ^^ {
      case p ~ opt => p :: opt.getOrElse(Nil)
    }
  lazy val maybeSeqPat: PackratParser[List[Pattern]] =
    rep1sep(maybeStarPat, ",") <~ opt(",")
  lazy val maybeStarPat: PackratParser[Pattern] = starPat | pattern
  lazy val starPat: PackratParser[Pattern] = "*" ~> patCaptureTarget ^^ {
    case x => MatchStar(Some(x))
  } | "*" ~> wildcardPat ^^^ MatchStar(None)
  lazy val mapPat: PackratParser[Pattern] =
    "{" ~ "}" ^^^ MatchMapping() |
    "{" ~> doubleStarPat <~ opt(",") ~ "}" ^^ {
      case x => MatchMapping(name = Some(x))
    } | "{" ~> itemsPat ~ ("," ~> doubleStarPat) <~ opt(",") ~ "}" ^^ {
      case map ~ x => MatchMapping(map, Some(x))
    } | "{" ~> itemsPat <~ opt(",") ~ "}" ^^ { MatchMapping(_) }
  lazy val itemsPat: PackratParser[List[(Expr,Pattern)]] = rep1sep(kvPat, ",")
  lazy val kvPat: PackratParser[(Expr, Pattern)] =
    (literalExpr | attr) ~ (":" ~> pattern) ^^ {
      case e ~ p => (e, p)
    }
  lazy val doubleStarPat: PackratParser[Id] = "**" ~> patCaptureTarget
  lazy val classPat: PackratParser[Pattern] =
    nameOrAttr <~ "(" ~ ")" ^^ { MatchClass(_) } |
    nameOrAttr ~ ("(" ~> posPats <~ opt(",") ~ ")") ^^ {
      case e ~ pos => MatchClass(e, pos)
    } | nameOrAttr ~ ("(" ~> keywordPats <~ opt(",") ~ ")") ^^ {
      case e ~ keyword => MatchClass(e, map = keyword)
    } | nameOrAttr ~ ("(" ~> posPats <~ ",") ~ keywordPats <~ opt(",") ~ ")" ^^ {
      case e ~ pos ~ keyword => MatchClass(e, pos, keyword)
    }
  lazy val posPats: PackratParser[List[Pattern]] = rep1sep(pattern, ",")
  lazy val keywordPats: PackratParser[List[(Id, Pattern)]] = rep1sep(keywordPat, ",")
  lazy val keywordPat: PackratParser[(Id, Pattern)] = id ~ ("=" ~> pattern) ^^ {
    case x ~ p => (x, p)
  }


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
  lazy val funcDef: PackratParser[Stmt] = (
    funcDefRaw |
    decorators ~ funcDefRaw ^^ {
      case dl ~ (f: FunDef) => f.copy(decos = dl)
      case dl ~ (f: AsyncFunDef) => f.copy(decos = dl)
    }
  )
  lazy val funcDefRaw: PackratParser[Stmt] = "def" ~> id ~ ("(" ~> opt(params) <~ ")") ~
    opt("->" ~> expression) ~ (":" ~> opt(funcTypeComment)) ~ block ^^ {
      case x ~ Some(ps) ~ tyopt ~ ftyopt ~ b => FunDef(Nil, x, ps, tyopt, ftyopt, b)
      case x ~ None ~ tyopt ~ ftyopt ~ b => FunDef(Nil, x, Args(), tyopt, ftyopt, b) // TODO empty Args?
    } |
    (("async" ~ "def") ~> id) ~ ("(" ~> opt(params) <~ ")") ~ opt("->" ~> expression) ~ opt(":" ~> funcTypeComment) ~ block ^^ {
      case x ~ Some(ps) ~ tyopt ~ ftyopt ~ b => AsyncFunDef(Nil, x, ps, tyopt, ftyopt, b)  
      case (((x ~ None) ~ tyopt) ~ ftyopt) ~ b => AsyncFunDef(Nil, x, Args(), tyopt, ftyopt, b) // TODO empty Args?
    }
  // case class Args(posOnlys: List[(Arg, Option[Expr])], normArgs: List[(Arg, Option[Expr])], varArg: Option[Arg], keyOnlys: List[(Arg, Option[Expr])], kwArg: Option[Arg]) 
  lazy val params:  PackratParser[Args] = parameters
  lazy val parameters: PackratParser[Args] = (
    slashNoDefault ~ rep(paramNoDefault) ~ rep(paramWithDefault) ~ opt(starEtc) ^^ {
      case snl ~ pnl ~ pwl ~ kopt => {
        val posonly: List[(Arg, Option[Expr])] = snl.map(a => (a, None))
        val midl: List[(Arg, Option[Expr])] = pnl.map(a => (a, None))
        kopt match {
          case Some(args) => args.copy(posOnlys = posonly, normArgs = midl)
          case None => Args(posonly, midl)
        }
      }
    } |
    slashWithDefault ~ rep(paramWithDefault) ~ opt(starEtc) ^^ {
      case (snl, swl) ~ pwl ~ kopt => {
        val posNoDefault: List[(Arg, Option[Expr])] = snl.map(a => (a, None))
        val posonly = posNoDefault ++ swl
        kopt match {
          case Some(args) => args.copy(posOnlys = posonly, normArgs = pwl)
          case None => Args(posonly, pwl)
        }
      }
    } |
    rep1(paramNoDefault) ~ rep(paramWithDefault) ~ opt(starEtc) ^^ {
      case pnl ~ pwl ~ kopt => {
        val normNoDefault: List[(Arg, Option[Expr])] = pnl.map(a => (a, None))
        val normargs = normNoDefault ++ pwl
        kopt match {
          case Some(args) => args.copy(normArgs = normargs)
          case None => Args(Nil, normargs)
        }
      }
    } |
    rep1(paramWithDefault) ~ opt(starEtc) ^^ {
      case pwl ~ kopt => {
        kopt match {
          case Some(args) => args.copy(normArgs = pwl)
          case None => Args(Nil, pwl)
        }
      }
    } |
    starEtc
  )
  lazy val slashNoDefault: PackratParser[List[Arg]] = (
    rep1(paramNoDefault) <~ ("/" ~ ",") |
    rep1(paramNoDefault) <~ ("/" ~ guard(")"))
  )
  // positional only args
  lazy val slashWithDefault: PackratParser[(List[Arg], List[(Arg, Option[Expr])])] = (
    rep(paramNoDefault) ~ rep1(paramWithDefault) <~ ("/" ~ ",") ^^ { case nl ~ wl => (nl, wl) } |
    rep(paramNoDefault) ~ rep1(paramWithDefault) <~ ("/" ~ guard(")")) ^^ { case nl ~ wl => (nl, wl) }
  )
  // vararg, keyword only arg, keywords arg
  lazy val starEtc: PackratParser[Args] = (
    "*" ~> paramNoDefault ~ rep(paramMaybeDefault) ~ opt(kwds) ^^ { 
      case a ~ pl ~ kopt => Args(Nil, Nil, Some(a), pl, kopt)
    } |
    ("*" ~ ",") ~> rep1(paramMaybeDefault) ~ opt(kwds) ^^ {
      case pl ~ kopt => Args(keyOnlys = pl, kwArg = kopt) 
    } |
    kwds ^^ {a => Args(kwArg = Some(a))} 
  )
  lazy val kwds: PackratParser[Arg] = "**" ~> paramNoDefault
  // Arg: id, annotation, typecomment / Expr: default value
  lazy val paramNoDefault: PackratParser[Arg] = (
    (param <~ ",") ~ opt(typeComment) ^^ { case (x, aopt) ~ tyopt => Arg(x, aopt, tyopt) } |
    param ~ opt(typeComment) <~ guard(")") ^^ { case (x, aopt) ~ tyopt => Arg(x, aopt, tyopt) }
  )
  lazy val paramWithDefault: PackratParser[(Arg, Option[Expr])] = (
    (param ~ default <~ ",") ~ opt(typeComment) ^^ { case ((x, aopt) ~ dexp) ~ tyopt => (Arg(x, aopt, tyopt), Some(dexp)) } |
    (param ~ default) ~ opt(typeComment) <~ guard(")") ^^ { case ((x, aopt) ~ dexp) ~ tyopt => (Arg(x, aopt, tyopt), Some(dexp))  }
  )
  lazy val paramMaybeDefault: PackratParser[(Arg, Option[Expr])] = (
    (param ~ opt(default) <~ ",") ~ opt(typeComment) ^^ { case ((x, aopt) ~ dopt) ~ tyopt => (Arg(x, aopt, tyopt), dopt)  } |
    (param ~ opt(default)) ~ opt(typeComment) <~ guard(")") ^^ { case ((x, aopt) ~ dopt) ~ tyopt => (Arg(x, aopt, tyopt), dopt) } 
  )
  lazy val param: PackratParser[(Id, Option[Expr])] = id ~ opt(annotation) ^^ { case x ~ eopt => (x, eopt) }
  lazy val annotation: PackratParser[Expr] = ":" ~> expression
  lazy val default: PackratParser[Expr] = "=" ~> expression
  lazy val decorators: PackratParser[List[Expr]] = rep1("@" ~> namedExpr <~ "\n") 

  /////////////////////////////////
  // Class def
  /////////////////////////////////
  // case class ClassDef(decos: List[Expr], name: Id, exprs: List[Expr], kwds: List[Kwarg], body: List[Stmt]) extends Stmt 
  lazy val classDef: PackratParser[Stmt] = (
    decorators ~ classDefRaw ^^ {
      case dl ~ cdr => cdr.copy(decos = dl)
    }
    | classDefRaw
  )
  lazy val classDefRaw: PackratParser[ClassDef] =
    ("class" ~> id) ~ opt("(" ~> opt(arguments) <~ ")") ~ (":" ~> block) ^^ {
      case x ~ None ~ b => ClassDef(Nil, x, Nil, Nil, b) 
      case x ~ Some(Some((el, kl))) ~ b => ClassDef(Nil, x, el, kl, b)
      case x ~ Some(None) ~ b => ClassDef(Nil, x, Nil, Nil, b) 
    }

  /////////////////////////////////
  // Block : List of Stmt
  /////////////////////////////////
  lazy val block: PackratParser[List[Stmt]] = (
    ("\n" ~ indent) ~> statements <~ dedent | 
    simpleStmts ^^ {
      case OnelineStmt(sl) =>  sl
      case s => List(s)
    }
  )

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
    "Listcomp" -> listcomp,
    "Tuple" -> tuple,
    "Set" -> set,
    "Setcomp" -> setcomp,
    "Dict" -> dict,
    "Dictcomp" -> dictcomp,
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

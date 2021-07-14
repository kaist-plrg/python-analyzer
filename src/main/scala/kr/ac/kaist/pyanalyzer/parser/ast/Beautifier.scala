package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.ast._

object Beautifier {
  implicit lazy val nodeApp: App[Node] = (app, node) => node match {
    case id: Id => app ~ id.name
    case const: Const => constApp(app, const)
    case op: Op => opApp(app, op)
    case comprehension: Comprehension => comprehensionApp(app, comprehension)
    case e: Expr => exprApp(app, e)
    case argument: Argument => argumentApp(app, argument)
    case stmt: Stmt => stmtApp(app, stmt)
    case pattern: Pattern => patternApp(app, pattern)
    case module: Module => moduleApp(app, module)
    case alias: Alias => aliasApp(app, alias)
    case withItem: WithItem => withItemApp(app, withItem)
    case handler: ExcHandler => excApp(app, handler)
    case node =>
      println(node)
      ???
  }

  implicit lazy val moduleApp: App[Module] = (app, node) => node match {
    case Module(stmtList, _) =>
      lsApp(app, stmtList)
  }

  implicit lazy val patternApp: App[Pattern] = (app, pattern) => pattern match {
    case MatchValue(e)  => app ~ e
    case MatchSingleton(c) => app ~ c
    case MatchSeq(pl) => 
      implicit val lApp = ListApp[Pattern](sep =  ", ")
      app ~ "[" ~ pl ~ "]"
    case MatchStar(nopt) => 
      app ~ "*" ~ &("", nopt, "")
    case MatchMapping(map, nopt) =>
      implicit val mapApp: App[(Expr, Pattern)] = {
        case (app, (e, p)) => app ~ e ~ " : " ~ p
      }
      implicit val lApp = ListApp[(Expr, Pattern)](sep = ", ")
      app ~ "{" ~ map ~ &(",", nopt, "") ~ "}"
    case MatchClass(ce, pl, map) =>
      implicit val mapApp: App[(Id, Pattern)] = {
        case (app, (x, p)) => app ~ x ~ " = " ~ p
      }
      implicit val plApp = ListApp[Pattern]("", ", ", ", ")
      implicit val mlApp = ListApp[(Id, Pattern)]("", ", ", ", ")
      app ~ ce ~ "(" ~ pl ~ map ~ ")"
    case MatchAs(popt, x) =>
      app ~ &("", popt, " as ") ~ x
    case MatchOr(pl) =>
      implicit val plApp = ListApp[Pattern](sep = " | ")
      app ~ pl
    case MatchWildcard =>
      app ~ "_"
    case MatchGroup(p) =>
      app ~ "(" ~ p ~ ")"
  }

  implicit lazy val aliasApp: App[Alias] = (app, alias) => alias match {
    case Alias(nl, asOpt) =>
      implicit val nlApp = ListApp[Id](sep = ".")
      app ~ nl ~ &(" as ", asOpt, "")
  }

  implicit lazy val withItemApp: App[WithItem] = (app, withItem) => withItem match {
    case WithItem(e, aopt) =>
      app ~ e ~ &(" as ", aopt, "")
  }

  implicit lazy val excApp: App[ExcHandler] = {
    case (app, ExcHandler(eOpt, xOpt, body)) =>
      app ~ "except" ~ &(" ", eOpt) ~ &(" as ", xOpt) ~ ":" ~ *(body)
  }

  // TODO
  implicit val lsApp = ListApp[Stmt]()

  implicit lazy val stmtApp: App[Stmt] = (app, stmt) => stmt match {
    case FunDef(decos, name, args, retType, tyExpr, body) =>
      implicit val leApp: App[List[Expr]] = (app, le) =>
        le.foldLeft(app)((app, e) => app ~ "@" ~ e ~ app.newLine)
      app ~ decos ~ "def " ~ name ~ "(" ~ args ~ ")" ~ &("->", retType) ~ ":" ~
        &(app.newLine +"", tyExpr) ~ *(body)
    case AsyncFunDef(decos, name, args, retType, tyExpr, body) =>
      app ~ "async " ~ FunDef(decos, name, args, retType, tyExpr, body)
    case ClassDef(decos, name, exprs, kwds, body) =>
      implicit val leApp = ListApp[Expr]("", ", ", ", ")
      implicit val klApp = ListApp[Kwarg]("", ", ", ", ")
      decos.foldLeft(app)((app, e) => app ~ "@" ~ e ~ app.newLine) ~
        "class " ~ name ~ "(" ~ exprs ~ kwds ~ ")" ~ ":" ~
        *(body)
    case ReturnStmt(e) => app ~ "return " ~ &(opt = e) ~ app.newLine
    case DelStmt(le) =>
      implicit val lApp = ListApp[Expr](sep = ", ")
      app ~ "del " ~ le ~ app.newLine
    case AssignStmt(targets, e, ty) =>
      targets.foldLeft(app)((app, target) => app ~ target ~ " = ") ~
        e ~ &("", ty) ~ app.newLine
    case AugAssign(target, op, e) =>
      app ~ target ~ " " ~ op ~ "= " ~ e ~ app.newLine
    case AnnAssign(target, ann, e) =>
      app ~ target ~ ": " ~ ann ~ &(" = ", e) ~ app.newLine
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      app ~ "for " ~ forExpr ~ " in " ~ inExpr ~ ":" ~ &("", ty) ~
        *(doStmt) ~ *(elseStmt, "else:")
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      app ~ "async " ~ ForStmt(ty, forExpr, inExpr, doStmt, elseStmt)
    case WhileStmt(cond, body, elseStmt) =>
      app ~ "while " ~ cond ~ ":" ~ *(body) ~ *(elseStmt, "else:")
    case IfStmt(cond, thenStmt, elseStmt) =>
      app ~ "if " ~ cond ~ ":" ~ *(thenStmt)
      elseStmt match {
        case Nil => app
        case s :: Nil if s.isInstanceOf[IfStmt] => app ~ "el" ~ s
        case stmts => app ~ "else:" ~ *(stmts)
      }
    case WithStmt(ty, items, body) =>
      implicit val lApp = ListApp[WithItem](sep = ", ")
      app ~ "with " ~ items ~ ":" ~ &("", ty) ~ *(body)
    case AsyncWithStmt(ty, items, body) =>
      app ~ "async " ~ WithStmt(ty, items, body)
    case MatchStmt(e, cases) =>
      implicit val caseApp: App[MatchCase] = {
        case (app, MatchCase(pat, cond, body)) =>
          app ~ "case " ~ pat ~ &(" if ", cond) ~ ":" ~ *(body)
      }
      implicit val lcApp = ListApp[MatchCase]()
      app ~ "match " ~ e ~ ":" ~ *(cases)
    case RaiseStmt(e, from) =>
      app ~ "raise " ~ &(opt = e) ~ &(" from ", from) ~ app.newLine
    case TryStmt(body, handlers, elseStmt, finStmt) =>
      implicit val lApp = ListApp[ExcHandler]()
      app ~ "try:" ~ *(body) ~ handlers ~ *(elseStmt, "else:") ~ *(finStmt, "finally:")
    case AssertStmt(c, opt) => app ~ "assert " ~ c ~ &(",", opt, "") ~ app.newLine
    case ImportStmt(aliases) =>
      implicit val lApp = ListApp[Alias](sep = ", ")
      app ~ "import " ~ aliases ~ app.newLine
    case ImportFromStmt(level, from, aliases) =>
      implicit val lxApp = ListApp[Id]("", ".", "")
      implicit val laApp = ListApp[Alias](sep = ", ")
      val aliasesApp: Update = app => aliases match {
        case Nil => app ~ "*"
        case l => app ~ l
      }
      // TODO: consider ellipsis
      app ~ "from " ~ ("."*level) ~ from ~ " import " ~ aliasesApp ~ app.newLine
    case GlobalStmt(xl) =>
      implicit val lApp = ListApp[Id](sep = ", ")
      app ~ "global " ~ xl ~ app.newLine
    case NonlocalStmt(xl) =>
      implicit val lApp = ListApp[Id](sep = ", ")
      app ~ "nonlocal " ~ xl ~ app.newLine
    case ExprStmt(e) => app ~ e ~ app.newLine
    case PassStmt => app ~ "pass" ~ app.newLine
    case BreakStmt => app ~ "break" ~ app.newLine
    case ContinueStmt => app ~ "continue" ~ app.newLine
    case OnelineStmt(sl) =>
      sl.foldLeft(app)((app, stmt) =>
        app ~ stmt ~ app.dropIndent ~ app.pop ~ "; "
      ) ~ app.pop.pop ~ app.newLine
  }

  implicit lazy val constApp: App[Const] = (app, c) => c match {
    case NoneLiteral => app ~ "None"
    case IntLiteral(i) => app ~ s"$i"
    case FloatLiteral(f) => app ~ s"$f"
    case ComplexLiteral(c) => app ~ s"${c}j"
    case StringLiteral(s) => app ~ s""""$s""""
    case BooleanLiteral(b) => app ~ (if (b) "True" else "False")
    case TupleLiteral(tup) =>
      implicit val lApp = ListApp[Const](sep = ", ")
      app ~ "(" ~ tup ~ ")"
    case Ellipsis => app ~ "..."
  }

  implicit lazy val exprApp: App[Expr] = (app, expr) => expr match {
    case BoolExpr(op, lhs, rhs) => app ~ lhs ~ " " ~ op ~ " " ~ rhs
    case NamedExpr(lhs, rhs) => app ~ lhs ~ " := " ~ rhs
    case BinaryExpr(op, lhs, rhs) => app ~ lhs ~ " " ~ op ~ " " ~ rhs
    case UnaryExpr(op, e) => app ~ op ~ " " ~ e
    case LambdaExpr(args, e) => app ~ "lambda " ~ args ~ " : " ~ e
    case IfExpr(e1, c, e2) => app ~ e1 ~ " if " ~ c ~ " else " ~ e2
    case DictExpr(lp, dstar) =>
      implicit val pApp: App[(Expr, Expr)] = {
        case (app, (k, v)) => app ~ k ~ ": " ~ v
      }
      implicit val lpApp = ListApp[(Expr, Expr)]("", ", ", ", ")
      implicit val lApp = ListApp[Expr]("", ", ", ", ")
      app ~ "{" ~ lp ~ dstar ~ "}"
    case SetExpr(set) =>
      implicit val lApp = ListApp[Expr](sep = ", ")
      app ~ "{" ~ set ~ "}"
    case ListExpr(l) =>
      implicit val lApp = ListApp[Expr](sep = ", ")
      app ~ "[" ~ l ~ "]"
    case TupleExpr(tup) =>
      val isSlices = !tup.forall(e => !e.isInstanceOf[Slice])
      val wrapper = if (isSlices) ("", "") else ("(", ")")
      lazy val tupApp = tup match {
        case head :: Nil => app ~ head ~ ","
        case tup =>
          implicit val lApp = ListApp[Expr](sep = ", ")
          app ~ tup
      }
      app.wrap(wrapper)(tupApp)
    case ListComp(target, comp) =>
      implicit val lApp = ListApp[Comprehension](sep = " ")
      app ~ "[" ~ target ~ " " ~ comp ~ "]"
    case SetComp(target, comp) =>
      implicit val lApp = ListApp[Comprehension](sep = " ")
      app ~ "{" ~ target ~ " " ~ comp ~ "}"
    case DictComp(p, comp) =>
      implicit val pApp: App[(Expr, Expr)] = {
        case (app, (k, v)) => app ~ k ~ ": " ~ v
      }
      implicit val lApp = ListApp[Comprehension](sep = " ")
      app ~ "{" ~ p ~ " " ~ comp ~ "}"
    case GenComp(target, comp) =>
      implicit val lApp = ListApp[Comprehension](sep = " ")
      app ~ "(" ~ target ~ " "  ~ comp ~ ")"
    case AwaitExpr(e) => app ~ "await " ~ e
    case YieldExpr(opt) => app ~ "yield " ~ &(opt = opt)
    case YieldFromExpr(e) => app ~ "yield from " ~ e
    case CompExpr(h, lp) =>
      implicit val pApp: App[(CompOp, Expr)] = {
        case (app, (op, e)) => app ~ op ~ " " ~ e
      }
      implicit val lApp = ListApp[(CompOp, Expr)](" ", " ")
      app ~ h ~ lp
    case Call(f, g :: Nil, Nil) if g.isInstanceOf[GenComp] => app ~ f ~ g
    case Call(f, le, lk) =>
      implicit val leApp = ListApp[Expr]("", ", ", ", ")
      implicit val lkApp = ListApp[Kwarg]("", ", ", ", ")
      app ~ f ~ "(" ~ le ~ lk ~ ")"
    case FormattedValue(lhs, n, rhs) => ???
    case JoinedStr(le) => ???
    case EConst(c) => app ~ c
    case Attribute(e, f) => app ~ e ~ "." ~ f
    case Subscript(e, s) => app ~ e ~ "[" ~ s ~ "]"
    case Starred(e) => app ~ "*" ~ e
    case DoubleStarred(e) => app ~ "**" ~ e
    case EName(x) => app ~ x
    case Slice(lb, ub, step) => app ~ &(opt = lb) ~ ":" ~ &(opt = ub) ~ &(":", step)
    case GroupExpr(e) => app ~ "(" ~ e ~ ")"
  }

  implicit lazy val comprehensionApp: App[Comprehension] = {
    case (app, Comprehension(target, inExpr, ifExpr, async)) =>
      implicit val lApp = ListApp[Expr](" if ", " if ")
      app ~ (if (async) "async " else "") ~
        "for " ~ target ~ " in " ~ inExpr ~ ifExpr
  }

  implicit lazy val argumentApp: App[Argument] = (app, argument) => argument match {
    case Args(pos, norm, varArg, key, kwarg) =>
      implicit val pApp: App[(Arg, Option[Expr])] = {
        case (app, (arg, opt)) => app ~ arg ~ &("=", opt)
      }
      implicit val lApp = ListApp[(Arg, Option[Expr])]("", ", ", ", ")
      val star = if (varArg.isInstanceOf[None.type] && key.nonEmpty) "*, " else ""
      app ~ ^("", pos, "/, ") ~ norm ~ star ~ &("*", varArg, ", ") ~
        key ~ &("**", kwarg, ", ")
    case Arg(x, ann, ty) => app ~ x ~ &(": ", ann) // TODO Add type comment
    case Kwarg(opt, e) => app ~ &("", opt, "=") ~ e
  }

  implicit lazy val opApp: App[Op] = (app, op) => op match {
    case OLShift => app ~ "<<"
    case ORShift => app ~ ">>"
    case OAdd => app ~ "+"
    case OSub => app ~ "-"
    case OMul => app ~ "*"
    case ODiv => app ~ "/"
    case OIDiv => app ~ "//"
    case OMod => app ~ "%"
    case OAt => app ~ "@"
    case OPow => app ~ "**"
    case OBAnd => app ~ "&"
    case OBOr => app ~ "|"
    case OBXor => app ~ "^"
    case UPlus => app ~ "+"
    case UMinus => app ~ "-"
    case UInv => app ~ "~"
    case UNot => app ~ "not"
    case CEq => app ~ "=="
    case CNeq => app ~ "!="
    case CLte => app ~ "<="
    case CLt => app ~ "<"
    case CGte => app ~ ">="
    case CGt => app ~ ">"
    case CNotIn => app ~ "not in"
    case CIn => app ~ "in"
    case CIsNot => app ~ "is not"
    case CIs => app ~ "is"
    case OAnd => app ~ "and"
    case OOr => app ~ "or"
  }
}

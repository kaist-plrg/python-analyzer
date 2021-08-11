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
    case dictItem: DictItem => dictItemApp(app, dictItem)
    case node =>
      println(node)
      ???
  }

  implicit lazy val dictItemApp: App[DictItem] = (app, item) => item match {
    case KVPair(k, v) => app ~ k ~ ": " ~ v
    case DoubleStarred(e) => app ~ "**" ~ #=(e, 6)
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
      app ~ "*" ~ ?(nopt)
    case MatchMapping(map, nopt) =>
      implicit val mapApp: App[(Expr, Pattern)] = {
        case (app, (e, p)) => app ~ e ~ " : " ~ p
      }
      implicit val lApp = ListApp[(Expr, Pattern)](sep = ", ")
      app ~ "{" ~ map ~ ?(nopt, ",") ~ "}"
    case MatchClass(ce, pl, map) =>
      implicit val mapApp: App[(Id, Pattern)] = {
        case (app, (x, p)) => app ~ x ~ " = " ~ p
      }
      implicit val plApp = ListApp[Pattern]("", ", ", ", ")
      implicit val mlApp = ListApp[(Id, Pattern)]("", ", ", ", ")
      app ~ ce ~ "(" ~ pl ~ map ~ ")"
    case MatchAs(popt, x) =>
      app ~ ?(popt, "", " as ") ~ x
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
      app ~ nl ~ ?(asOpt, " as ")
  }

  implicit lazy val withItemApp: App[WithItem] = (app, withItem) => withItem match {
    case WithItem(e, aopt) =>
      app ~ e ~ ?(aopt, " as ")
  }

  implicit lazy val excApp: App[ExcHandler] = {
    case (app, ExcHandler(eOpt, xOpt, body)) =>
      app ~ "except" ~ ?(eOpt, " ") ~ ?(xOpt, " as ") ~ ":" ~ wrap(body)
  }

  // TODO
  implicit val lsApp = ListApp[Stmt]()

  implicit lazy val stmtApp: App[Stmt] = (app, stmt) => stmt match {
    case FunDef(decos, name, args, retType, tyExpr, body) =>
      implicit val leApp: App[List[Expr]] = (app, le) =>
        le.foldLeft(app)((app, e) => app ~ "@" ~ e ~ app.newLine)
      app ~ decos ~ "def " ~ name ~ "(" ~ args ~ ")" ~ ?(retType, "->") ~ ":" ~
        ?(tyExpr, app.newLine) ~ wrap(body)
    case AsyncFunDef(decos, name, args, retType, tyExpr, body) =>
      app ~ "async " ~ FunDef(decos, name, args, retType, tyExpr, body)
    case ClassDef(decos, name, exprs, kwds, body) =>
      implicit val leApp = LEApp("", ", ", ", ")
      implicit val klApp = ListApp[Kwarg]("", ", ", ", ")
      decos.foldLeft(app)((app, e) => app ~ "@" ~ e ~ app.newLine) ~
        "class " ~ name ~ "(" ~ exprs ~ kwds ~ ")" ~ ":" ~
        wrap(body)
    case ReturnStmt(e) => app ~ "return " ~ ?(e) ~ app.newLine
    case DelStmt(le) =>
      implicit val lApp = LEApp(sep = ", ")
      app ~ "del " ~ le ~ app.newLine
    case AssignStmt(targets, e, ty) =>
      targets.foldLeft(app)((app, target) => app ~ target ~ " = ") ~
        e ~ ?(ty) ~ app.newLine
    case AugAssign(target, op, e) =>
      app ~ target ~ " " ~ op ~ "= " ~ e ~ app.newLine
    case AnnAssign(target, ann, e) =>
      app ~ target ~ ": " ~ ann ~ ?(e, " = ") ~ app.newLine
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      app ~ "for " ~ forExpr ~ " in " ~ inExpr ~ ":" ~ ?(ty) ~
        wrap(doStmt) ~ wrap(elseStmt, "else:")
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      app ~ "async " ~ ForStmt(ty, forExpr, inExpr, doStmt, elseStmt)
    case WhileStmt(cond, body, elseStmt) =>
      app ~ "while " ~ cond ~ ":" ~ wrap(body) ~ wrap(elseStmt, "else:")
    case IfStmt(cond, thenStmt, elseStmt) =>
      app ~ "if " ~ cond ~ ":" ~ wrap(thenStmt)
      elseStmt match {
        case Nil => app
        case s :: Nil if s.isInstanceOf[IfStmt] => app ~ "el" ~ s
        case stmts => app ~ "else:" ~ wrap(stmts)
      }
    case WithStmt(ty, items, body) =>
      implicit val lApp = ListApp[WithItem](sep = ", ")
      app ~ "with " ~ items ~ ":" ~ ?(ty) ~ wrap(body)
    case AsyncWithStmt(ty, items, body) =>
      app ~ "async " ~ WithStmt(ty, items, body)
    case MatchStmt(e, cases) =>
      implicit val caseApp: App[MatchCase] = {
        case (app, MatchCase(pat, cond, body)) =>
          app ~ "case " ~ pat ~ ?(cond, " if ") ~ ":" ~ wrap(body)
      }
      implicit val lcApp = ListApp[MatchCase]()
      app ~ "match " ~ e ~ ":" ~ wrap(cases)
    case RaiseStmt(e, from) =>
      app ~ "raise " ~ ?(e) ~ ?(from, " from ") ~ app.newLine
    case TryStmt(body, handlers, elseStmt, finStmt) =>
      implicit val lApp = ListApp[ExcHandler]()
      app ~ "try:" ~ wrap(body) ~ handlers ~ wrap(elseStmt, "else:") ~
        wrap(finStmt, "finally:")
    case AssertStmt(c, opt) => app ~ "assert " ~ c ~ ?(opt, ",") ~ app.newLine
    case ImportStmt(aliases) =>
      implicit val lApp = ListApp[Alias](sep = ", ")
      app ~ "import " ~ aliases ~ app.newLine
    case ImportFromStmt(level, from, aliases) =>
      implicit val lxApp = ListApp[Id]("", ".")
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
    case StringLiteral(s) => {
      if (s contains "\n") app ~ s"""\"\"\"$s\"\"\""""
      else app ~ s""""$s""""
    }
    case BooleanLiteral(b) => app ~ (if (b) "True" else "False")
    case TupleLiteral(tup) =>
      implicit val lApp = ListApp[Const](sep = ", ")
      app ~ "(" ~ tup ~ ")"
    case Ellipsis => app ~ "..."
  }

  // you MUST see the precednece help in parser/ast/Expr
  // before implement/understand exprApp
  implicit lazy val exprApp: App[Expr] = (app, expr) => {
    // default outer precedence
    implicit val precedence: Int = expr.precedence
    expr match {
      case BoolExpr(op, lhs, rhs) =>
        implicit val precedence = expr.precedence + 1
        app ~ lhs ~ " " ~ op ~ " " ~ rhs
      case NamedExpr(lhs, rhs) =>
        implicit val precedence = 15
        app ~ lhs ~ " := " ~ rhs
      // Power operator is right-associative
      case BinaryExpr(OPow, lhs, rhs) =>
        app ~ #=(lhs, precedence + 1) ~ " ** " ~ rhs
      case BinaryExpr(op, lhs, rhs) =>
        app ~ lhs ~ " " ~ op ~ " " ~ #=(rhs, precedence + 1)
      case UnaryExpr(UNot, e) => app ~ UNot ~ " " ~ e
      case UnaryExpr(op, e) => app ~ op ~ e
      case LambdaExpr(args, e) => app ~ "lambda " ~ args ~ " : " ~ e
      case IfExpr(e1, c, e2) =>
        app ~ e1 ~ " if " ~ c ~ " else " ~ e2
      case DictExpr(map) =>
        implicit val precedence = 1
        implicit lazy val lApp = ListApp[DictItem](sep = ", ")
        app ~ "{" ~ map ~ "}"
      case SetExpr(set) =>
        implicit val precedence = 1
        implicit val lApp = LEApp(sep = ", ")
        app ~ "{" ~ set ~ "}"
      case ListExpr(l) =>
        implicit val precedence = 1
        implicit val lApp = LEApp(sep = ", ")
        app ~ "[" ~ l ~ "]"
      case TupleExpr(tup) =>
        implicit val precedence = 1
        lazy val tupApp: Update = app => tup match {
          case head :: Nil => app ~ head ~ ","
          case tup =>
            implicit val lApp = LEApp(sep = ", ")
            app ~ tup
        }
        app ~ tupApp
      case ListComp(target, comp) =>
        implicit val precedence = 1
        implicit val lApp = ListApp[Comprehension](sep = " ")
        app ~ "[" ~ target ~ " " ~ comp ~ "]"
      case SetComp(target, comp) =>
        implicit val precedence = 1
        implicit val lApp = ListApp[Comprehension](sep = " ")
        app ~ "{" ~ target ~ " " ~ comp ~ "}"
      case DictComp(p, comp) =>
        implicit val precedence = 1
        implicit val pApp: App[(Expr, Expr)] = {
          case (app, (k, v)) => app ~ k ~ ": " ~ v
        }
        implicit val lApp = ListApp[Comprehension](sep = " ")
        app ~ "{" ~ p ~ " " ~ comp ~ "}"
      case GenComp(target, comp) =>
        implicit val precedence = 1
        implicit val lApp = ListApp[Comprehension](sep = " ")
        app ~ "(" ~ target ~ " "  ~ comp ~ ")"
      case AwaitExpr(e) =>
        implicit val precedence = 15
        app ~ "await " ~ e
      // yield/yield from need default parenthesis
      // TODO: Remove it in the statement
      case YieldExpr(opt) =>
        implicit val precedence = 1
        app ~ "(yield" ~ ?(opt, " ") ~ ")"
      case YieldFromExpr(e) =>
        implicit val precedence = 1
        app ~ "(yield from " ~ e ~ ")"
      case CompExpr(h, lp) =>
        implicit val precedence = expr.precedence + 1
        implicit val pApp: App[(CompOp, Expr)] = {
          case (app, (op, e)) => app ~ op ~ " " ~ e
        }
        implicit val lApp = ListApp[(CompOp, Expr)](" ", " ")
        app ~ h ~ lp
      case Call(f, g :: Nil, Nil) if g.isInstanceOf[GenComp] => app ~ f ~ g
      case Call(f, le, lk) =>
        implicit val precedence = 1
        implicit val leApp = LEApp("", ", ", ", ")
        implicit val lkApp = ListApp[Kwarg]("", ", ", ", ")
        app ~ #=(f, 15) ~ "(" ~ le ~ lk ~ ")"
      case FormattedValue(lhs, n, rhs) => ???
      case JoinedStr(le) => ???
      case EConst(c) => app ~ c
      case Attribute(e, f) => app ~ e ~ "." ~ f
      case Subscript(e, s) =>
        // each slice(Expr) in slices needs parenthesis
        // slices(TupleExpr) doesn't need parenthesis
        implicit val precedence = s match {
          // empty tuple edge case
          case TupleExpr(tup) if tup.isEmpty => 1
          case TupleExpr(tup) => if (tup.forall(e => e match {
            case Starred(_) => false
            case _ => true
          })) 0 else 1
          case _ => 0
        }
        app ~ #=(e, 15) ~ "[" ~ s ~ "]"
      case Starred(e) =>
        implicit val precedence = 6
        app ~ "*" ~ e
      case EName(x) => app ~ x
      case Slice(lb, ub, step) =>
        implicit val precedence = 1
        app ~ ?(lb) ~ ":" ~ ?(ub) ~ ?(step, ":")
      case GroupExpr(e) => app ~ "(" ~ e ~ ")"
    }
  }

  implicit lazy val comprehensionApp: App[Comprehension] = {
    case (app, Compre(target, inExpr, ifExpr)) =>
      implicit val precedence = 2
      implicit val lApp = LEApp(" if ", " if ")
      // empty tuple edge case
      target match {
        case TupleExpr(tup) if tup.isEmpty =>
          app ~ "for " ~ #=(target, 1) ~ " in " ~ inExpr ~ ifExpr
        case _ =>
          app ~ "for " ~ #=(target, 0) ~ " in " ~ inExpr ~ ifExpr
      }
    case (app, AsyncCompre(target, inExpr, ifExpr)) =>
      app ~ "async " ~ Compre(target, inExpr, ifExpr)
  }

  implicit lazy val argumentApp: App[Argument] = (app, argument) => argument match {
    case Args(pos, norm, varArg, key, kwarg) =>
      implicit val pApp: App[(Arg, Option[Expr])] = {
        case (app, (arg, opt)) => app ~ arg ~ ?(opt, "=")
      }
      implicit val lApp = ListApp[(Arg, Option[Expr])]("", ", ", ", ")
      val star = if (varArg.isInstanceOf[None.type] && key.nonEmpty) "*, " else ""
      app ~ ?(pos, "", "/, ") ~ norm ~ star ~ ?(varArg, "*", ", ") ~
        key ~ ?(kwarg, "**", ", ")
    case Arg(x, ann, ty) => app ~ x ~ ?(ann, ": ") // TODO Add type comment
    case NormalKwarg(id, e) => app ~ id ~ "=" ~ e
    case DoubleStarredKwarg(e) => app ~ "**" ~ e
    case Keyword(a, v) => a match {
      case None => app ~ "**" ~ v 
      case Some(x) => app ~ x ~ "=" ~ v
    }
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

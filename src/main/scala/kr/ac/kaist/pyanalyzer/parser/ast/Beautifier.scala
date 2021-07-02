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
    case node =>
      println(node)
      ???
  }

  implicit lazy val stmtApp: App[Stmt] = (app, stmt) => stmt match {
    case PassStmt => app ~ "pass"
    case BreakStmt => app ~ "break"
    case ContinueStmt => app ~ "continue"
    case GlobalStmt(xl) =>
      implicit val lApp = ListApp[Id](sep = ", ")
      app ~ "global " ~ xl
    case NonlocalStmt(xl) =>
      implicit val lApp = ListApp[Id](sep = ", ")
      app ~ "nonlocal " ~ xl
    case AssertStmt(c, opt) =>
      implicit val oApp = OptApp[Expr](", ")
      app ~ "assert" ~ c ~ opt
    case _ => ???
  }

  implicit lazy val constApp: App[Const] = (app, c) => c match {
    case NoneLiteral => app ~ "None"
    case IntLiteral(i) => app ~ s"$i"
    case FloatLiteral(f) => app ~ s"$f"
    case ComplexLiteral(c) => app ~ s"${c}j"
    case StringLiteral(s) => app ~ s""""$s""""
    case BooleanLiteral(b) => app ~ (if (b) "True" else "False")
    case TupleLiteral(tup) =>
      implicit val lApp = ListApp[Const]("(", ", ", ")")
      app ~ tup
    case Ellipsis => app ~ "..."
  }

  implicit lazy val exprApp: App[Expr] = (app, expr) => expr match {
    case BoolExpr(op, lhs, rhs) => app ~ lhs ~ " " ~ op ~ " " ~ rhs
    case NamedExpr(lhs, rhs) => app ~ lhs ~ " := " ~ rhs
    case BinaryExpr(op, lhs, rhs) => app ~ lhs ~ " " ~ op ~ " " ~ rhs
    case UnaryExpr(op, e) => app ~ op ~ " " ~ e
    case LambdaExpr(args, e) => app ~ args ~ e
    case IfExpr(e1, c, e2) => app ~ e1 ~ " if " ~ c ~ " else " ~ e2
    case DictExpr(lp, dstar) =>
      implicit val pApp: App[(Expr, Expr)] = {
        case (app, (k, v)) => app ~ k ~ ": " ~ v
      }
      implicit val lpApp = ListApp[(Expr, Expr)]("{", ", ")
      implicit val lApp = ListApp[Expr]("", ", ", "}")
      val sep = sepOpt(lp, dstar, ", ")
      app ~ lp ~ sep ~ dstar
    case SetExpr(set) =>
      implicit val lApp = ListApp[Expr]("{", ", ", "}")
      app ~ set
    case ListExpr(l) =>
      implicit val lApp = ListApp[Expr]("[", ", ", "]")
      app ~ l
    case TupleExpr(tup) => tup match {
      case head :: Nil => app ~ "(" ~ head ~ ",)"
      case tup =>
        implicit val lApp = ListApp[Expr]("(", ", ", ")")
        app ~ tup
      }
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
    case YieldExpr(opt) =>
      implicit val oApp = OptApp[Expr]()
      app ~ "yield " ~ opt
    case YieldFromExpr(e) => app ~ "yield from " ~ e
    case CompExpr(h, lp) =>
      implicit val pApp: App[(CompOp, Expr)] = {
        case (app, (op, e)) => app ~ op ~ " " ~ e
      }
      implicit val lApp = ListApp[(CompOp, Expr)](" ", " ")
      app ~ h ~ lp
    case Call(f, le, lk) =>
      implicit val leApp = ListApp[Expr](sep = ", ")
      implicit val lkApp = ListApp[Kwarg](sep = ", ")
      val sep = sepOpt(le, lk, ", ")
      app ~ f ~ "(" ~ le ~ sep ~ lk ~ ")"
    case FormattedValue(lhs, n, rhs) => ???
    case JoinedStr(le) => ???
    case EConst(c) => app ~ c
    case Attribute(e, f) => app ~ e ~ "." ~ f
    case Subscript(e, s) => app ~ e ~ "[" ~ s ~ "]"
    case Starred(e) => app ~ "*" ~ e
    case DoubleStarred(e) => app ~ "**" ~ e
    case EName(x) => app ~ x
    case Slice(lb, ub, step) =>
      implicit val oApp = OptApp[Expr]()
      app ~ lb ~ ":" ~ ub
      step match {
        case Some(e) => app ~ ":" ~ e
        case None => app
      }
    case GroupExpr(e) => app ~ "(" ~ e ~ ")"
  }

  implicit lazy val comprehensionApp: App[Comprehension] = {
    case (app, Comprehension(target, inExpr, ifExpr, async)) =>
      implicit val lApp: App[List[Expr]] = (app, l) => {
        for (e <- l) app ~ " if " ~ e; app
      }
      app ~ (if (async) "async " else "") ~
        "for " ~ target ~ " in " ~ inExpr ~ ifExpr
  }

  implicit lazy val argumentApp: App[Argument] = (app, argument) => argument match {
    case Args(_, _, _, _, _) => ???
    case Arg(x, ann, ty) => app ~ x // TODO: Add annotation and type
    case Kwarg(opt, e) =>
      implicit val oApp = OptApp[Id](right = "=")
      app ~ opt ~ e
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

  def sepOpt[T, U](l1: List[T], l2: List[U], sep: String): Option[String] =
    if (l1.nonEmpty && l2.nonEmpty) Some(sep) else None
}

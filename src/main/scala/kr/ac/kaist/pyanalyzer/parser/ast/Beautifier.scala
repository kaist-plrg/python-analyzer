package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.ast._

object Beautifier {
  implicit lazy val nodeApp: App[Node] = (app, node) => node match {
    case e: Expr => exprApp(app, e)
    case node: Op => opApp(app, node)
    case _ => ???
  }

  implicit lazy val exprApp: App[Expr] = (app, expr) => expr match {
    case EEmpty => ???
    case AId(x) => app ~ x
    case AStringLiteral(str) => app ~ s""""$str""""
    case ABytesLiteral(b) => app ~ s"b$b"
    case AIntLiteral(i) => app ~ s"$i"
    case AFloatLiteral(f) => app ~ s"$f"
    case AImagLiteral(i) => app ~ s"${i}j"
    case ABool(b) => app ~ (if (b) "True" else "False")
    case ANone => app ~ "None"
    case ListExpr(l) =>
      implicit val lApp = ListApp[Expr]("[", ", ", "]")
      app ~ l
    case TupleExpr(tup) => tup match {
      case head :: Nil => app ~ "( " ~ head ~ ",)"
      case tup =>
        implicit val lApp = ListApp[Expr]("( ", ", ", ")")
        app ~ tup
      }
    case SetExpr(set) =>
      implicit val lApp = ListApp[Expr]("{", ", ", "}")
      app ~ set
    case DictExpr(map, given) => ???
    case KVPair(k, v) => ???
    case EAttrRef(prim, ref) => ???
    case ESubscript(prim, exprs) => ???
    case Slicing(prim, args) => ???
    case Call(prim, args) => ???
    case Slice(lb, ub, step) => ???
    case UnaryExpr(op, e) => app ~ op ~ " " ~ e
    case BinaryExpr(op, lhs, rhs) => app ~ lhs ~ " " ~ op ~ " " ~ rhs
    case CompareExpr(h, lp) =>
      implicit val pApp: App[(COp, Expr)] = {
        case (app, (op, e)) => app ~ op ~ " " ~ e
      }
      implicit val lApp = ListApp[(COp, Expr)](" ", " ")
      app ~ h ~ lp
    case AssignExpr(id, e) => app ~ id ~ " := " ~ e
    case CondExpr(c, t, e) => app ~ c ~ " if " ~ t ~ " else " ~ e
    case AwaitExpr(e) => app ~ "await " ~ e
    case LambdaExpr(param, e) => ???
    case StarExpr(e) => app ~ "*" ~ e
    case CompFor(targets, inExpr, ifExpr, async) => ???
    case ListCompExpr(target, comp) => ???
    case SetCompExpr(target, comp) => ???
    case DictCompExpr(kv, comp) => ???
    case YieldExpr(el) => ???
    case GroupExpr(e) => app ~ "( " ~ e ~ ")"
    case GenExpr(target, comp) => ???
  }

  implicit lazy val opApp: App[Op] = (app, op) => op match {
    case AugAssignOp(op) => ???
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
    case LNot => app ~ "not"
    case LAnd => app ~ "and"
    case LOr => app ~ "or"
    case UPlus => app ~ "+"
    case UMinus => app ~ "-"
    case UInv => app ~ "~"
  }
}

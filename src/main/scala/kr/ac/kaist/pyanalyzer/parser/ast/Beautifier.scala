package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.ast._

object Beautifier {
  implicit lazy val nodeApp: App[Node] = (app, node) => node match {
    case stmt: Stmt => stmtApp(app, stmt)
    case e: Expr => exprApp(app, e)
    case item: DictItem => dictItemApp(app, item)
    case arg: Arg => argApp(app, arg)
    case param: Param => paramApp(app, param)
    case op: Op => opApp(app, op)
    case _ => ???
  }

  implicit lazy val stmtApp: App[Stmt] = (app, stmt) => stmt match {
    case PassStmt => app ~ "pass"
    case BreakStmt => app ~ "break"
    case ContinueStmt => app ~ "continue"
    case GlobalStmt(xl) =>
      implicit val lApp = ListApp[AId](sep = ", ")
      app ~ "global " ~ xl
    case NonlocalStmt(xl) =>
      implicit val lApp = ListApp[AId](sep = ", ")
      app ~ "nonlocal " ~ xl
    case YieldStmt(e) => app ~ e
    case AssertStmt(c, opt) =>
      app ~ "assert" ~ c
      opt.map(info => app ~ ", " ~ info); app
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
      case head :: Nil => app ~ "(" ~ head ~ ",)"
      case tup =>
        implicit val lApp = ListApp[Expr]("(", ", ", ")")
        app ~ tup
      }
    case SetExpr(set) =>
      implicit val lApp = ListApp[Expr]("{", ", ", "}")
      app ~ set
    case DictExpr(map) =>
      implicit val dApp = ListApp[DictItem]("{", ", ", "}")
      app ~ map
    case EAttrRef(prim, ref) => app ~ prim ~ "." ~ ref
    case ESubscript(prim, e) =>
      implicit val lApp = ListApp[Expr]("[", ", ", "]")
      app ~ prim ~ e
    case Call(prim, args) =>
      implicit val lApp = ListApp[Arg]("(", ", ", ")")
      app ~ prim ~ args
    case Slice(lb, ub, step) =>
      app ~ lb ~ ":" ~ ub
      step.map(x => app ~ ":" ~ x); app
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
    case LambdaExpr(param, e) => 
      app ~ "lambda "
      val keyParam = param.find(_.isInstanceOf[KeyParam])
      val starSepParam = keyParam.map(k => {
        val index = param.indexOf(k)
        (param.slice(0, index), param.slice(index, param.length))
      })
      implicit val lApp = ListApp[Param](sep = ", ")
      implicit val plApp: App[(List[Param], List[Param])] = {
        case (app, (Nil, l2)) => app ~ "*, " ~ l2
        case (app, (l1, l2)) => app ~ l1 ~ ", *, " ~ l2
      }
      (starSepParam match {
        case None => app ~ param
        case s => app ~ s
      }) ~ ": " ~ e
    case StarExpr(e) => app ~ "*" ~ e
    case DStarExpr(e) => app ~ "**" ~ e
    case CompFor(target, inExpr, ifExpr, async) =>
      implicit val lApp: App[List[Expr]] = (app, l) => l match {
        case Nil => app
        case l => for (e <- l) app ~ " if " ~ e; app
      }
      app ~ (if (async) "async " else "") ~
        "for " ~ target ~ " in " ~ inExpr ~ ifExpr
    case ListCompExpr(target, comp) =>
      implicit val lApp = ListApp[Expr](" ", " ")
      app ~ "[" ~ target ~ comp ~ "]"
    case SetCompExpr(target, comp) =>
      implicit val lApp = ListApp[Expr](" ", " ")
      app ~ "{" ~ target ~ comp ~ "}"
    case DictCompExpr(item, comp) =>
      implicit val lApp = ListApp[Expr](" ", " ")
      app ~ "{" ~ item ~ comp ~ "}"
    case YieldExpr(e) => app ~ "yield " ~ e
    case YieldFromExpr(e) => app ~ "yield from " ~ e
    case GroupExpr(e) => app ~ "(" ~ e ~ ")"
    case GenExpr(target, comp) =>
      implicit val lApp = ListApp[Expr](" ", " ")
      app ~ "(" ~ target  ~ comp ~ ")"
  }

  implicit lazy val dictItemApp: App[DictItem] = (app, item) => item match {
    case KvPair(k, v) => app ~ k ~ ": " ~ v
    case DStarItem(e) => app ~ e
  }

  implicit lazy val argApp: App[Arg] = (app, arg) => arg match {
    case NormalArg(e) => app ~ e
    case KeyArg(x, e) => app ~ x ~ "=" ~ e
  }

  implicit lazy val paramApp: App[Param] = (app, param) => param match {
    case PosParam(id, default) =>
      app ~ id; default.map(x => app ~ " = " ~ x); app
    case KeyParam(id, default) =>
      app ~ id; default.map(x => app ~ " = " ~ x); app
    case ArbPosParam(id) => app ~ "*" ~ id
    case ArbKeyParam(id) => app ~ "**" ~ id
  }

  implicit lazy val opApp: App[Op] = (app, op) => op match {
    case AugOp(op) => ???
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

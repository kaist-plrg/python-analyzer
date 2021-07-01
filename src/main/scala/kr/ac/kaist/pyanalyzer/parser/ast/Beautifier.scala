package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.ast._

object Beautifier {
  implicit lazy val nodeApp: App[Node] = (app, node) => node match {
    case stmt: Stmt => stmtApp(app, stmt)
    case e: Expr => exprApp(app, e)
    // case item: DictItem => dictItemApp(app, item)
    // case arg: Arg => argApp(app, arg)
    // case param: Param => paramApp(app, param)
    case op: Op => opApp(app, op)
    case _ => ???
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
      app ~ "assert" ~ c
      opt.map(info => app ~ ", " ~ info); app
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
//    case LambdaExpr(param, e) => 
//      app ~ "lambda "
//      val keyParam = param.find(_.isInstanceOf[KeyParam])
//      val starSepParam = keyParam.map(k => {
//        val index = param.indexOf(k)
//        (param.slice(0, index), param.slice(index, param.length))
//      })
//      implicit val lApp = ListApp[Param](sep = ", ")
//      implicit val plApp: App[(List[Param], List[Param])] = {
//        case (app, (Nil, l2)) => app ~ "*, " ~ l2
//        case (app, (l1, l2)) => app ~ l1 ~ ", *, " ~ l2
//      }
//      (starSepParam match {
//        case None => app ~ param
//        case s => app ~ s
//      }) ~ ": " ~ e
//    case IfExpr(e1, c, e2) => app ~ e1 ~ " if " ~ c ~ " else " ~ e2
    /*
    case DictExpr(map) =>
      implicit val dApp = ListApp[DictItem]("{", ", ", "}")
      app ~ map
    */
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
//    case DictComp(item, comp) =>
//      implicit val lApp = ListApp[Comprehension](sep = " ")
//      app ~ "{" ~ item ~ " " ~ comp ~ "}"
    case GenComp(target, comp) =>
      implicit val lApp = ListApp[Comprehension](sep = " ")
      app ~ "(" ~ target ~ " "  ~ comp ~ ")"
    case AwaitExpr(e) => app ~ "await " ~ e
    case YieldExpr(opt) => app ~ "yield " ~ opt
    case YieldFromExpr(e) => app ~ "yield from " ~ e
    case CompExpr(h, lp) =>
      implicit val pApp: App[(CompOp, Expr)] = {
        case (app, (op, e)) => app ~ op ~ " " ~ e
      }
      implicit val lApp = ListApp[(CompOp, Expr)](" ", " ")
      app ~ h ~ lp
//    case Call(prim, args) =>
//      implicit val lApp = ListApp[Arg]("(", ", ", ")")
//      app ~ prim ~ args
    case FormattedValue(lhs, n, rhs) => ???
    case JoinedStr(le) => ???
    case Attribute(e, f) => app ~ e ~ "." ~ f
    case Subscript(e, s) =>
      implicit val lApp = ListApp[Expr]("[", ", ", "]")
      app ~ e ~ s
    case Starred(e) => app ~ "*" ~ e
    case EName(x) => app ~ x
    case Slice(lb, ub, step) =>
      app ~ lb ~ ":" ~ ub
      step.map(x => app ~ ":" ~ x); app
    case GroupExpr(e) => app ~ "(" ~ e ~ ")"
  }

  implicit lazy val comprehensionApp: App[Comprehension] = {
    case (app, Comprehension(target, inExpr, ifExpr, async)) =>
      implicit val lApp: App[List[Expr]] = (app, l) => l match {
        case Nil => app
        case l => for (e <- l) app ~ " if " ~ e; app
      }
      app ~ (if (async) "async " else "") ~
        "for " ~ target ~ " in " ~ inExpr ~ ifExpr
  }

  /*
  implicit lazy val dictItemApp: App[DictItem] = (app, item) => item match {
    case KvPair(k, v) => app ~ k ~ ": " ~ v
    case DStarItem(e) => app ~ e
  }
  */

//  implicit lazy val argApp: App[Arg] = (app, arg) => arg match {
//    case NormalArg(e) => app ~ e
//    case KeyArg(x, e) => app ~ x ~ "=" ~ e
//  }

  /*
  implicit lazy val paramApp: App[Param] = (app, param) => param match {
    case PosParam(id, default) =>
      app ~ id; default.map(x => app ~ " = " ~ x); app
    case KeyParam(id, default) =>
      app ~ id; default.map(x => app ~ " = " ~ x); app
    case ArbPosParam(id) => app ~ "*" ~ id
    case ArbKeyParam(id) => app ~ "**" ~ id
  }
  */

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

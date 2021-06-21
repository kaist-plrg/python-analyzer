package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.ast._

object Beautifier {
  implicit lazy val nodeApp: App[Node] = (app, node) => node match {
    case node: Expr => exprApp(app, node)
    case _ => ???
  }

  implicit lazy val exprApp: App[Expr] = (app, expr) => expr match {
    case AId(x) => app ~ x
    case AStringLiteral(str) => app ~ s""""$str""""
    case ABytesLiteral(b) => app ~ s"b$b"
    case AIntLiteral(i) => app ~ s"$i"
    case AFloatLiteral(f) => app ~ s"$f"
    case AImagLiteral(i) => app ~ s"${i}j"
    case ABool(b) => app ~ s"$b"
    case ANone => app ~ "None"
    case ListExpr(l) =>
      implicit val imp = ListApp[Expr]("[", ", ", "]")
      app ~ l
    case _ => ???
  }
}

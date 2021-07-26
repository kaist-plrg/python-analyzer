package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.parser.ast._

class Appender(tab: String = "  ") {
  // TODO: Refactor
  import Appender._

  val sb: StringBuilder = new StringBuilder
  private var k = 0
  def indent = { k += 1; tab }
  def dedent = {
    k -= 1
    val length = sb.length
    sb.delete(length - 2, length)
    ""
  }
  def pop = { sb.deleteCharAt(sb.length - 1); this }
  def dropIndent = { sb.delete(sb.length - 2*k, sb.length); this }
  def newLine = "\n" + tab * k
  override def toString: String = sb.toString

  // main appender
  def ~(str: String): Appender = { sb ++= str; this }
  def ~[T](x: T)(implicit app: App[T]): Appender = app(this, x)
  def ~(f: Update): Appender = f(this)
  def ~(app: Appender) = app

  // appender with precedence
  // if outer precedence is larger than inner precedence, add parenthesis
  def ~(e: Expr)(implicit app: App[Expr], precedence: Int = 1): Appender = {
    if (precedence > e.precedence) sb += '('
    app(this, e)
    if (precedence > e.precedence) sb += ')'
    this
  }
  // make a verbous call to avoid ambiguous method overloading
  // TODO: Remove verbous call
  def appendExpr(e: Expr)(implicit app: App[Expr], precedence: Int = 1): Appender = this ~ e
  // append expression with given outer precedence
  def ~(preOp: #=)(implicit app: App[Expr]): Appender = {
    val #=(expr, outerPrecedence) = preOp
    implicit val precedence = outerPrecedence
    this ~ expr
  }

  // appender helper
  // optionally append
  def ~[T](opt: ?[T])(implicit app: App[T]): Appender = opt match {
    case ?(Some(e: Expr), l, r, precedence) =>
      this ~ l
      implicit val pre = precedence
      implicit val eApp = app.asInstanceOf[App[Expr]]
      appendExpr(e)
      this ~ r
    case ?(None, l, r, _) => this
    case ?(Some(v), l, r, _) => this ~ l ~ v ~ r
  }
  def ~[T](block: wrap[T])(implicit app: App[T]): Appender = block match {
    case wrap(Nil, l, r) => this
    case wrap(list, l, r) => this ~ l ~ newLine ~ indent ~ list ~ dedent ~ r
  }
}
object Appender {
  // Scala value appender
  type App[T] = (Appender, T) => Appender
  type Update = Appender => Appender

  // nothing appender
  def nothingApp[T]: App[T] = (app, t) => app

  // Scala value appender
  implicit lazy val stringApp: App[String] = _ ~ _
  implicit lazy val intApp: App[Int] = _ ~ _.toString

  implicit val optStrApp: App[Option[String]] = (app, opt) => opt match {
    case Some(str) => app ~ str
    case None => app
  }

  // list appender with separator
  // if it is Nil, left and right is not appended
  def ListApp[T](
    left: String = "",
    sep: String = "",
    right: String = ""
  )(implicit tApp: App[T]): App[List[T]] = (app, list) => list match {
    case Nil => app
    case hd :: tl =>
      app ~ left ~ hd
      for (t <- tl) app ~ sep ~ t
      app ~ right
  }
  // ListApp only for Expr
  // make a verbous call to avoid ambiguous method overloading
  // TODO: Remove verbous call
  def LEApp(
    left: String = "",
    sep: String = "",
    right: String = ""
  )(implicit tApp: App[Expr], precedence: Int = 1): App[List[Expr]] = (app, list) => list match {
    case Nil => app
    case hd :: tl =>
      app ~ left ~ hd
      for (t <- tl) app ~ sep ~ t
      app ~ right
  }

  // optionally append with left, right wrapper
  // pre is used only for expr
  // TODO: Remove pre and implicitly pass precedence
  case class ?[T](opt: Option[T], l: String = "", r: String = "", pre: Int = 1)
  case class wrap[T](block: T, l: String = "", r: String = "")
  // expression with outer precedence 'precedence'
  case class #=(e: Expr, precedence: Int)

  implicit def toListOpt[T](l: List[T]): Option[List[T]] = l match {
    case Nil => None
    case l => Some(l)
  }
}

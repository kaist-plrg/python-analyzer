package kr.ac.kaist.pyanalyzer.util

class Appender(tab: String = "  ") {
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
  def newLine = "\n" + tab * k
  override def toString: String = sb.toString
  def ~(str: String): Appender = { sb ++= str; this }
  def ~[T](x: T)(implicit app: App[T]): Appender = app(this, x)
  def ~(f: Update): Appender = f(this)
  def ~(app: Appender) = app
  def ~[T](opt: &[T])(implicit app: App[T]): Appender = opt match {
    case &(l, None, r) => this
    case &(l, Some(v), r) => this ~ l ~ v ~ r
  }
  def ~[T](opt: ^[T])(implicit app: App[List[T]]): Appender = opt match {
    case ^(l, Nil, r) => this
    case ^(l, list, r) => this ~ l ~ list ~ r
  }
  def ~[T](block: *[T])(implicit app: App[T]): Appender =
    this ~ newLine ~ indent ~ block.block ~ dedent

  // TODO: Refactor
  def wrap(
    lr: (String, String) = ("", "")
  )(f: => Unit): Appender = {
    val (l, r) = lr
    this ~ l
    f
    this ~ r
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

  // lists with separator
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

  def sepOpt[T, U](l1: List[T], l2: List[U], sep: String): Option[String] =
    if (l1.nonEmpty && l2.nonEmpty) Some(sep) else None

  // TODO: Give appropriate name!
  case class &[T](l: String = "", opt: Option[T], r: String = "")
  case class *[T](block: T)
  case class ^[T](l: String = "", list: List[T], r: String = "")
}

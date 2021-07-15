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
  def dropIndent = { sb.delete(sb.length - 2*k, sb.length); this }
  def newLine = "\n" + tab * k
  override def toString: String = sb.toString
  def ~(str: String): Appender = { sb ++= str; this }
  def ~[T](x: T)(implicit app: App[T]): Appender = app(this, x)
  def ~(f: Update): Appender = f(this)
  def ~(app: Appender) = app
  def ~[T](opt: ?[T])(implicit app: App[T]): Appender = opt match {
    case ?(None, l, r) => this
    case ?(Some(v), l, r) => this ~ l ~ v ~ r
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

  case class ?[T](opt: Option[T], l: String = "", r: String = "")
  case class wrap[T](block: T, l: String = "", r: String = "")

  implicit def toListOpt[T](l: List[T]): Option[List[T]] = l match {
    case Nil => None
    case l => Some(l)
  }
}

package kr.ac.kaist.pyanalyzer.util

class Appender(tab: String = "  ") {
  import Appender._

  val sb: StringBuilder = new StringBuilder
  var k = 0
  def indent = tab * k
  override def toString: String = sb.toString
  def ~(str: String): Appender = { sb ++= str; this }
  def ~[T](x: T)(implicit app: App[T]): Appender = app(this, x)
  def ~(f: Update): Appender = f(this)
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

  // option appender
  def OptApp[T](
    left: String = "",
    right: String = ""
  )(implicit vApp: App[T]): App[Option[T]] = (app, opt) => opt match {
    case Some(v) => app ~ left ~ v ~ right
    case None => app
  }

  // lists with separator
  def ListApp[T](
    left: String = "",
    sep: String = "",
    right: String = ""
  )(implicit tApp: App[T]): App[List[T]] = (app, list) => list match {
    case Nil => app ~ left ~ right
    case hd :: tl =>
      app ~ left ~ hd
      for (t <- tl) app ~ sep ~ t
      app ~ right
  }
}

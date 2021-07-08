package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.util.Appender
import scala.Console._

object Useful {
  def beautify[T](t: T)(implicit app: Appender.App[T]): String =
    app(new Appender, t).toString
  
  def assert(f: => Boolean)(msg: String): Unit = {
    if (!f) { throw new RuntimeException(msg) }
  }

  // Console display styles
  def colored(color: String)(msg: String): String = s"${color}${msg}${RESET}"
  def bolded(msg: String): String = s"${BOLD}${msg}${RESET}"
  final val GRAY: String = "\u001B[90m"
}

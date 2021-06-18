package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.util.Appender

object Useful {
  def beautify[T](t: T)(implicit app: Appender.App[T]): String =
    app(new Appender, t).toString
}

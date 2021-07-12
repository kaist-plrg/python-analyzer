package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.util.Appender
import scala.Console._
import java.io.File

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

  // File system utils
  def walkTree(filename: String): Iterable[File] = walkTree(new File(filename))
  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator: Iterator[File] =
        if (file.isDirectory) file.listFiles.iterator
        else Iterator.empty
    }
    Seq(file) ++ children.flatMap(walkTree(_))
  }
}

package kr.ac.kaist.pyanalyzer.util

import java.io._
import kr.ac.kaist.pyanalyzer._
import scala.Console._
import scala.sys.process._

object Useful {
  def beautify[T](t: T)(implicit app: Appender.App[T]): String =
    app(new Appender, t).toString
  
  def assert(f: => Boolean)(msg: String): Unit = {
    if (!f) { throw new RuntimeException(msg) }
  }

  // create directories
  def mkdir(name: String): Unit = new File(name).mkdirs

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

  ////////////////////////////////////////////
  // console and log printing related helpers
  ////////////////////////////////////////////
  
  // Console display styles
  def colored(color: String)(msg: String): String = s"${color}${msg}${RESET}"
  def bolded(msg: String): String = s"${BOLD}${msg}${RESET}"
  final val GRAY: String = "\u001B[90m"

// getting printwriter obj
  def getPrintWriter(filename: String): PrintWriter =
    new PrintWriter(new File(filename))

  // `setPrompt = true` to see message
  def prompt(s: String)(implicit setPrompt: Boolean, lw: PrintWriter): Unit = {
    if (setPrompt) println(s)
    // s might contain color codes for console output 
    lw.write(withoutColorCodes(s) + "\n")
  }

  def flushLog(endMsg: String = "\n")(implicit lw: PrintWriter): Unit = {
    lw.write(endMsg)
    lw.flush()
  }
  // dump given data to a file
  def dumpFile(data: Any, filename: String): Unit = {
    val nf = getPrintWriter(filename)
    nf.print(data)
    nf.close()
  }

  // removes color codes
  def withoutColorCodes(msg: String): String =
    msg.replaceAll("\u001B\\[[;\\d]*m", "");

  ////////////////////////////////////////////
  // command related helpers
  ////////////////////////////////////////////

  // execute shell command with given dir, default to CUR_DIR
  def executeCmd(given: String, dir: String = BASE_DIR, log: Boolean = false): Int = {
    if (log) println(s"[SHELL] $given")
    given.!
  }
}

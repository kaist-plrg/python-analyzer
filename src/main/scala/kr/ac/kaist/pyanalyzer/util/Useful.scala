package kr.ac.kaist.pyanalyzer.util

import java.io._
import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import scala.Console._
import scala.sys.process._
import scala.io.Source

object Useful {
  def beautify[T](t: T)(implicit app: Appender.App[T]): String = {
    val res = app(new Appender, t).toString
    t match {
      // default tuple doesn't have parenthesis, so need parenthesis
      case tup: TupleExpr => "(" + res + ")"
      // named expression need grouping parenthesis
      case tup: NamedExpr => "(" + res + ")"
      case _ => res
    }
  }
  
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
    lw.flush()
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

  // read string from file
  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val lines = try source.getLines mkString "\n" finally source.close()
    lines
  }

  // write string to file
  def writeFile(path: String, data: String): Unit = {
    val writer = new PrintWriter(new File(path))
    writer.write(data)
    writer.flush()
    writer.close()
  }

  // print exception stack trace
  def printStackTrace(e: Throwable): String = { 
    val sw = new StringWriter();
    e.printStackTrace(new PrintWriter(sw));
    sw.toString();
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

  def accUntil[T](e: T)(next: T => Option[T]): List[T] =
    next(e) match {
      case None => List(e) 
      case Some(n) => e +: accUntil(n)(next)
    }
}

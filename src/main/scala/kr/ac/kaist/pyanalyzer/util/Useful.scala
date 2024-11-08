package kr.ac.kaist.pyanalyzer.util

import java.io._
import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import scala.Console._
import scala.io.Source
import scala.sys.process._

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

  // `setPrompt = true` to see message
  def warning(setPrompt: Boolean, lw: PrintWriter) = (filename: String) =>
    (warning: String, code: String) => {
      val message = s"$YELLOW$warning\n$code\n$RESET"
      if (setPrompt) println(message)
      // s might contain color codes for console output 
      lw.write(withoutColorCodes(s"<$filename> $message"))
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

  // should overwrite the file
  def writeFile(path: File, data: String): Unit = {
    val writer = new PrintWriter(path)
    writer.write(data)
    writer.flush()
    writer.close()
  }



  // dump Info[Module] to files
  def dumpModuleToPath(mod: Module, path: File, name: String): Unit = {
    val prettyCode = beautify(mod)
    val newFilePath = new File(path, name + ".py")
    if (newFilePath.createNewFile()) {
      writeFile(newFilePath, prettyCode)
    } else {
      //throw new RuntimeException(s"unexpectedly existing ${newFilePath}")
      writeFile(newFilePath, prettyCode)
    }
  }

  def dumpInfoModule(info: Info[Module], dir: File): Unit = info match {
    case DirInfo(name, ds, fs) => {
      val newDirPath = new File(dir, name)
      if (newDirPath.mkdirs()) {
        val l: List[Info[Module]] = ds ++ fs
        l.foreach(info => dumpInfoModule(info, newDirPath))
      }
      else {
        //throw new RuntimeException(s"unexpectedly existing ${newDirPath}")
        val l: List[Info[Module]] = ds ++ fs
        l.foreach(info => dumpInfoModule(info, newDirPath))
      }
    }
    case FileInfo(name, mod) => dumpModuleToPath(mod, dir, name)
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
  def executeCmd(
    given: ProcessBuilder,
    dir: String = BASE_DIR,
    log: Boolean = false
  ): Int = {
    if (log) println(s"[SHELL] $given")
    given.!
  }

  def accUntil[T](e: T)(next: T => Option[T]): List[T] =
    next(e) match {
      case None => List(e) 
      case Some(n) => e +: accUntil(n)(next)
    }

  // Adding comments
  implicit class StmtsWrapper(stmts: List[Stmt]) {
    def addComment(c: String): List[Stmt] = Comment(c) +: stmts
  }

  implicit class StmtWrapper(stmt: Stmt) {
    def addComment(c: String): List[Stmt] = List(Comment(c), stmt)
  }
}

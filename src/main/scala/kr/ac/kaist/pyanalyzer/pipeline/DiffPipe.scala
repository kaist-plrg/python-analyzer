package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

case object DiffPipe extends Pipeline[(String, Module, Module), Unit] {
  def execute(p: (String, Module, Module)): Unit = {
    val (path, org, trans) = p
    dumpModuleToPath(org, new File(path), "org")
    dumpModuleToPath(trans, new File(path), "trans")
    try executeCmd(s"colordiff -u $path/org.py $path/trans.py")
    catch {
      case e: Throwable =>
        executeCmd(s"diff -u $path/org.py $path/trans.py")
        println(s"$YELLOW[Warning] install colordiff$RESET")
    }
    executeCmd(s"rm $path/org.py $path/trans.py")
  }
}

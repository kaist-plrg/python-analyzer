package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.{ Info }
import scala.Console._
import java.io.File

object ParsePath {
  def apply(optionMap: Map[String, String]): Unit = {
    optionMap.get("path") match {
      case Some(path) =>
        println(path)
        TransformRunner.runPipe(path)
      case None => println(s"${YELLOW}[Warning] no path is given$RESET")
    }
  }
}

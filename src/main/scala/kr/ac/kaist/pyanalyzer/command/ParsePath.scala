package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object ParsePath {
  def apply(optionMap: Map[String, String]): Unit = {
    optionMap.get("path") match {
      case Some(path) =>
        println(path)
        val resultAst: Module = TransformRunner.run(path)
        println(beautify(resultAst))
      case None => println(s"${YELLOW}[Warning] no path is given$RESET")
    }
  }
}

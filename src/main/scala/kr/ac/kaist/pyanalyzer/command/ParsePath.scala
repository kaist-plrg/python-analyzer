package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._

object ParsePath {
  def apply(optionMap: Map[String, String]): Unit = {
    val path = optionMap("path") 
    val resultAst: Module = TransformRunner.run(path)
    println(beautify(resultAst))
  }
}

package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.util.{ Info }

case object TransformPipe extends Pipeline[
  (Module, ClassOrder, APIType), 
  Module
] {
  def execute(
    p: (Module, ClassOrder, APIType)
  ): Module = {
    val (mainscript, order, api) = p
    Transformer(mainscript, order, api)
  }
}

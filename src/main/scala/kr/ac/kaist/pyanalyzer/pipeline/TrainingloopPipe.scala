package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Env

case object TrainingLoopPipe
  extends Pipeline[(Module, ClassOrder), (Module, ClassOrder, APIType)] {

  def execute(p: (Module, ClassOrder)): (Module, ClassOrder, APIType) = {
    val (mod, order) = p
    (mod, order, APIAnalyzer(mod, order).api)
  }
}

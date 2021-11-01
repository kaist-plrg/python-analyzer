package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Env

case object TrainingLoopPipe
  extends Pipeline[(Module, ClassOrder), (Module, ClassOrder, TLType)] {

  def execute(p: (Module, ClassOrder)): (Module, ClassOrder, TLType) = {
    val (mod, order) = p
    (mod, order, TrainingLoop(mod, order).tl)
  }
}

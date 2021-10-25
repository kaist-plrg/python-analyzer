package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer.Transformer

case object TransformPipe extends Pipeline[(Module, ClassOrder), Module] {
  def execute(p: (Module, ClassOrder)): Module = ???
}

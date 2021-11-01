package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env

case object TransformPipe extends Pipeline[(Module, ClassOrder, TLType), Module] {
  def execute(p: (Module, ClassOrder, TLType)): Module = {
    val (mod, order, tl) = p
    Transformer(mod, order, tl)
  }
}

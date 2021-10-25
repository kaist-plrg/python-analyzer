package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env

case object TransformPipe extends Pipeline[(Module, ClassOrder), Module] {
  def execute(p: (Module, ClassOrder)): Module = {
    val mod = p._1
    val order = p._2
    Transformer(mod, Env(Map(), order), ((x: String, y: String) => ()))
  }
}

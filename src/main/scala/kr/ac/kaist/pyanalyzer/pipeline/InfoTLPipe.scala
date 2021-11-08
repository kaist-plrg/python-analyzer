package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info }

case object InfoTLPipe 
extends Pipeline[(Info[Module], ClassOrder), Info[ModuleSummary]] {
  def execute(p: (Info[Module], ClassOrder)): Info[ModuleSummary] = {
    val (modInfo, order) = p
    modInfo.map(mod => TrainingLoop(mod, order))
  }
}

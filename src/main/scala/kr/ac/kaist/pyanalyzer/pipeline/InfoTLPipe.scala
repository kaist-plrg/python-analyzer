package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info }

case object InfoTLPipe 
extends Pipeline[(Info[Module], ClassOrder), Info[(Module, ModuleSummary)]] {
  def execute(p: (Info[Module], ClassOrder)): Info[(Module, ModuleSummary)] = {
    val (modInfo, order) = p
    modInfo.map(mod => (mod, TrainingLoop(mod, order)))
  }
}

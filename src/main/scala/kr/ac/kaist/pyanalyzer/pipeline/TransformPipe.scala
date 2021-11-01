package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info }

case object TransformPipe 
extends Pipeline
[((Info[(Module, ModuleSummary)], ClassOrder), Option[String]), Info[Module]] {
  def execute(
    p: ((Info[(Module, ModuleSummary)], ClassOrder), Option[String])
  ): Info[Module] = {
    val ((info, order), opt) = p   
    // TODO impl
    opt match {
      case Some(fname) => ??? // target file path given 
      case None => ??? // should find target file by ModuleSummary TLType
    }
  }
}

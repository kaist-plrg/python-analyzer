package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.util.{ Info }
import kr.ac.kaist.pyanalyzer.transformer.{ ClassOrder, ModuleSummary }
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline._
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline.PipelineOps

object TransformRunner {
  val subPipe: 
  Pipeline[Info[Module], (Info[(Module, ModuleSummary)], ClassOrder)] = 
    (idPipe ++ ClassPipe) >> (InfoTLPipe || idPipe[(Info[Module], ClassOrder)].snd)

  val transformPipe = // String -> Info[Module], transform applied
    (PathPipe ++ CheckFilePipe) // String -> (File, Option[String])
      .fstMap(ParsePipe) 
        // (File, Option[String]) 
        // -> (Info[Module], Option[String]) 
      .fstMap(subPipe) >> TransformPipe
        // (Info[Module], Option[String]) 
        // -> ((Info[(Module, ModuleSummary)], ClassOrder), Option[String])
        // -> Info[Module]
 
  def run(path: String): Info[Module] = transformPipe!!(path)
}

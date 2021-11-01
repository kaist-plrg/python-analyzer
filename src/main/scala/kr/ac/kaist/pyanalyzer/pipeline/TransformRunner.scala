package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline.PipelineOps

object TransformRunner {
  val transformPipe =
    PathPipe >> (ParsePipe || ClassPipe) >> TrainingLoopPipe >> TransformPipe
  def run(path: String): Module = transformPipe!!(path) 
}

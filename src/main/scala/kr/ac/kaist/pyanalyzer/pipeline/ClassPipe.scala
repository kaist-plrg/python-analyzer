package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.pipeline.Pipeline
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer.ClassAnalysis._
import kr.ac.kaist.pyanalyzer.util.Info
import java.io.File

case object ClassPipe extends Pipeline[Info[Module], ClassOrder] {
  def execute(info: Info[Module]): ClassOrder = {
    val res = infoModuleToOrder(info)
    println(s"-------CHA:------")
    println(res)
    println("-----------------------")
    res
  }
}

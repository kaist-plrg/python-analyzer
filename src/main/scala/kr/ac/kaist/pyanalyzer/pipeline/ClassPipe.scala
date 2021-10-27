package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.pipeline.Pipeline
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer.ClassAnalysis._
import kr.ac.kaist.pyanalyzer.util.Info
import java.io.File

case object ClassPipe extends Pipeline[File, ClassOrder] {
  def execute(path: File): ClassOrder = {
    val res = dirClassOrder(path) 
    println(s"-------CHA:$path------")
    println(res)
    println("-----------------------")
    res
  }
}

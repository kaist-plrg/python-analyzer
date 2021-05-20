package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser._

object PyAnalyzer {
  // main entry point
  def main(tokens: Array[String]): Unit = {
    val testParser = SourceParser("12344\n 123") 
    println(testParser)
  }
}

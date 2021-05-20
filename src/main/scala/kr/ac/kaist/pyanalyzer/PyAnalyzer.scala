package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser._

object PyAnalyzer {
  // main entry point
  def main(tokens: Array[String]): Unit = {
    val testParser = SourceParser(s"$PY_SOURCE_DIR/test01.py") 
    println(testParser)
  }
}

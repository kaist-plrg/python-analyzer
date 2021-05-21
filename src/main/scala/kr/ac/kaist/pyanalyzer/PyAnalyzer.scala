package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser._


object PyAnalyzer {
  // main entry point
  def main(args: Array[String]): Unit = {
    val tokens = SourceParser(s"$PY_SOURCE_DIR/test01.py") 
    Token.printTokens(tokens)
  }
}

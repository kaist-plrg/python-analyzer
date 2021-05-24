package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._

object PyAnalyzer {
  // main entry point
  def main(args: Array[String]): Unit = {
    val tokens: List[Stmt] = SourceParser(s"$PY_SOURCE_DIR/test01.py") 
    println(tokens)
  }
}

package kr.ac.kaist.pyanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import kr.ac.kaist.pyanalyzer.parser._

class TokenTest extends AnyFlatSpec {
  "Tokenizing" should "not throw error!" in {
    try {
      print(SourceParser(s"$PY_SOURCE_DIR/test01.py"))
    } catch {
      case e: Exception => assert(false)
    }
  }
}

package kr.ac.kaist.pyanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import kr.ac.kaist.pyanalyzer.parser._
import java.io.File

class TokenTest extends AnyFlatSpec {
  val testDir = new File(PY_SOURCE_DIR)
  for (test <- testDir.listFiles.filter(_.isFile))
   s"$test" should "not throw exception" in {
    try {
      print(SourceParser(s"$test"))
    } catch {
      case e: Exception => assert(false)
    }
  }
}

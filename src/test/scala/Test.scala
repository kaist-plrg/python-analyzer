package kr.ac.kaist.pyanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import kr.ac.kaist.pyanalyzer.parser._
import java.io.File

class TokenTest extends AnyFlatSpec {
  val sourceDir = new File(PY_SOURCE_DIR)
  for {
    category <- sourceDir.listFiles.filter(_.isDirectory)
    testDir <- category.listFiles.filter(_.isDirectory)
  }
    s"$testDir" should "not throw exception" in {
      try {
        print(SourceParser(s"$testDir/main.py"))
      } catch {
        case e: Exception =>
          if (!(e.getMessage contains "parsing fail")) {
            println(s"${e.getMessage}")
            println(s"${e.getStackTrace.mkString("\n")}")
          }
          assert(false)
      }
    }
}

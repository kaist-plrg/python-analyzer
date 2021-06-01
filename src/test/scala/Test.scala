package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import java.io.File

class TokenTest extends AnyFunSuite {
  val sourceDir = new File(PY_SOURCE_DIR)
  for {
    category <- sourceDir.listFiles.filter(_.isDirectory)
    testDir <- category.listFiles.filter(_.isDirectory)
  } {
    val testName = s"$testDir".split("/").reverse.slice(0,2).reverse.mkString("/")
    test(s"\n$testName") {
      try {
        print(SourceParser(s"$testDir/main.py"))
      } catch {
        case e: Exception =>
          if (!(e.getMessage contains "parsing fail")) {
            println(s"Unexpected Exception!")
            println(s"${e.getMessage}")
            println(s"${e.getStackTrace.mkString("\n")}")
          }
          assert(false)
      }
    }
  }
}

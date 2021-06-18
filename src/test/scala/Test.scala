package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.CheckProd
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import scala.util.Random._
import scala.Console._
import java.io.File

class ProdTest extends AnyFunSuite {

  val help =s"""
    Test each production.

    $RED****IMPORTANT****$RESET
    ${BLUE}Note that test case of each production is chosen randomly.
    So, if you want to retrive the testcase, copy and paste that in the parse repl.
    $RESET"""

  println(help)

  for (prod <- prodMap.keys)
    test(s"$prod") (if (!CheckProd(prod)) assert(false))

}

// Test the read code
// val sourceDir = new File(PY_SOURCE_DIR)
// for {
//   category <- sourceDir.listFiles.filter(_.isDirectory)
//   testDir <- category.listFiles.filter(_.isDirectory)
// } {
//   val testName = s"$testDir".split("/").reverse.slice(0,2).reverse.mkString("/")
//   test(s"\n$testName") {
//     try {
//       print(SourceParser(s"$testDir/main.py"))
//     } catch {
//       case e: Exception =>
//         if (!(e.getMessage contains "parsing fail")) {
//           println(s"Unexpected Exception!")
//           println(s"${e.getMessage}")
//           println(s"${e.getStackTrace.mkString("\n")}")
//         }
//         assert(false)
//     }
//   }
// }

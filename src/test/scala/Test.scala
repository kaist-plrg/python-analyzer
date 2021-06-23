package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.Grammar._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.Useful._
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

  val times = 10

  for ((prod, p) <- prodMap) test(s"$prod") {
    for {
      (t, i) <- PEG_Grammar(prod).zipWithIndex
      time <- 1 to times
    } {
      val test: String = t
      val CHECK = false
      if (CHECK) {
        println(s"<$prod$i>")
        println(test)
        println
      }
      val ast1 = doParse(p, test)
      val beautified1 = beautify(ast1)
      val ast2 = try doParse(p, beautified1) catch {
        case e: Exception =>
          throw new RuntimeException(s"Second parsing failed!\n\ntest:\n$test\n\nfirst:\n$beautified1\n\n$e")
      }
      val beautified2 = beautify(ast2)
      if (beautified1 != beautified2)
        throw new RuntimeException(s"Inconsistent result!\n\ntest:\n$test\n\nfirst:\n$beautified1\n\nsecond:\n$beautified2")
    }
  }

  def doParse(p: Parser[Node], t: String): Node =
    p(new PackratReader(TokenReader(parseText(t)))) match {
      case Success(res, rest) if rest.first == Newline => res
      case res => throw new RuntimeException(s"\ntest:\n$t\n\n$res")
    }
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

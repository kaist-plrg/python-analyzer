package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.Grammar._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer.tokenizeText
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

  val times = 30
  val depth = 10
  val CHECK = false

  for ((prod, p) <- prodMap) test(s"$prod") {
    for {
      (t, i) <- PEG_Grammar(prod).zipWithIndex
      time <- 1 to times
    } {
      val test = t.testWithDepth(depth) // test with depth
      // val test: String = t // test with only random
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

  def doParse(p: PackratParser[Node], t: String): Node =
    p(new PackratReader(TokenReader(tokenizeText(t)))) match {
      case Success(res, rest) if rest.first == NewlineToken() => res
      case res => throw new RuntimeException(s"\nParsing fail\ntest:\n$t\n\n$res")
    }
}

package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import java.io.File

class TokenTest extends AnyFunSuite {
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
  val prodMap = Map(
    "Atom" -> atom,
    "Atom_complex" -> atom,
  )

  val testMap = Map(
    "Atom" -> List(
      "True", "False",
      "id",
      """\"str\"""",
      "1", "1.0", "1j",
    ),
    "Atom_complex" -> List(
    //   tuple, group, genexp
    //   list, listcomp
    //   dict, set, dictcomp, setcomp
    ),
  )

  def prodTest(name: String, index: Int, parser: Parser[Node], test: String) =
    ProdTest(name + index, parser, test)

  for {
    (prod, parser) <- prodMap
    (t, index) <- testMap(prod).zipWithIndex
  } {
    val testname = s"$prod $index"
    test(testname) {
      try {
        println(s"<$testname>")
        prodTest(prod, index, parser, t)() match {
          case Success(res, rest) if rest.first == Newline =>
            println(s"$testname parsed!")
            println(res)
            println
          case res =>
            println(res)
            println
            throw new RuntimeException("parsing fail")
        }
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

case class ProdTest(name: String, p: Parser[Node], test: String) {
  def apply() = {
    val token = parseText(test)
    println(token)
    p(new PackratReader(TokenReader(token)))
  }

  lazy val parser = p <~ "\n"
}

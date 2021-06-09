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
    "Atom_base" -> atom,
    "Atom" -> atom,
    "Atom_complex" -> atom,
    "Primary" -> primary,
    "Bop" -> bop,
    "Term" -> term,
  )

  val partialTestMap: Map[String, List[testScript]] = Map(
    "Atom_base" -> List("atom"),
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
    "Primary" -> List(
      LazyBinding("Atom_base"),
    ),
    "Bop" -> List("*", "/", "//", "%", "@"),
    "Term" -> List(
      LazyBinding("Primary") + LazyBinding("Bop") + LazyBinding("Atom_base")
    ),
  )

  def toTestString(testScript: testScript): List[String] = testScript match {
    case Normal(test) => List(test)
    case LazyBinding(name) => partialTestMap(name).flatMap(toTestString)
    case +(a, b) => 
      for {
        aa <- toTestString(a)
        bb <- toTestString(b)
      } yield s"$aa $bb"
  }

  trait testScript {
    def +(rhs: testScript) = new +(this, rhs)
  }

  case class LazyBinding(name: String) extends testScript

  case class Normal(test: String) extends testScript

  case class +(a: testScript, b: testScript) extends testScript

  implicit def toTestScript(str: String): testScript = Normal(str)

  val testMap: Map[String, List[String]] = partialTestMap.map {
    case (key, list) => (key, list.flatMap(toTestString))
  }

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

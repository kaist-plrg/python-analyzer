package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import scala.util.Random
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
    "Atom_failed" -> atom,
    "Atom_complex" -> atom,
    "Primary" -> primary,
    "Primary_complex" -> primary,
    "AwaitPrimary" -> awaitPrimary,
    "Power" -> power,
    "Uop" -> uop,
    "Factor" -> factor,
    "Bop" -> bop,
    "Term" -> term,
    "Term_LR" -> term,
    "Sum" -> sum,
    "Sum_LR" -> sum,
  )

  val partialTestMap: Map[String, List[testScript]] = Map(
    "Atom" -> List(
      "True", "False",
      "id",
      """"str"""",
      "1"//, "1.0", "1j",
    ),
    "Atom_failed" -> List("1.0", "1j"),
    "Atom_complex" -> List(
      // tuple, group, genexp
      // list, listcomp
      // dict, set, dictcomp, setcomp
    ),
    "Primary" -> List(), // trivial case, use Atom_base
    "Primary_complex" -> List(
      // TODO
    ),
    "AwaitPrimary" -> List("await" ~ LazyBinding("Atom")),
    "Power" -> List(
      LazyBinding("Atom") ~ "**" ~ LazyBinding("Atom"),
      LazyBinding("Atom"),
    ),
    "Uop" -> List("+", "-", "~"),
    "Factor" -> List(
      LazyBinding("Uop") ~ LazyBinding("Power"), LazyBinding("Power"),
      LazyBinding("Atom"),
    ),
    "Bop" -> List("*", "/", "//", "%", "@"),
    "Term" -> List(
      LazyBinding("Factor") ~ LazyBinding("Bop") ~ LazyBinding("Factor"),
    ),
    "Term_LR" -> List(
      LazyBinding("Term") ~ LazyBinding("Bop") ~ LazyBinding("Factor"),
      LazyBinding("Factor"),
    ),
    "Sum" -> List(LazyBinding("Term") ~ "+" ~ LazyBinding("Term")),
    "Sum_LR" -> List(LazyBinding("Sum") ~ "+" ~ LazyBinding("Term")),
  )

  // TODO: Add test for invalid syntax
  // val partialSyntaxErrorMap = ???

  def toTestString(testScript: testScript): List[String] = testScript match {
    case Normal(test) => List(test)
    // test all possible way
    // case LazyBinding(name) => partialTestMap(name).flatMap(toTestString)
    // test one way in each production randomly
    case LazyBinding(name) => 
      val candidate = partialTestMap(name)
      toTestString(candidate(Random.nextInt(candidate.length)))
    case ~(a, b) => 
      for {
        aa <- toTestString(a)
        bb <- toTestString(b)
      } yield s"$aa $bb"
  }

  trait testScript {
    def ~(rhs: testScript) = new ~(this, rhs)
  }

  case class LazyBinding(name: String) extends testScript

  case class Normal(test: String) extends testScript

  case class ~(a: testScript, b: testScript) extends testScript

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
        println(t)
        prodTest(prod, index, parser, t)() match {
          case Success(res, rest) if rest.first == Newline =>
            println(s"parsing success")
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

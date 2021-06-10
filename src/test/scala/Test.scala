package kr.ac.kaist.pyanalyzer

import org.scalatest.funsuite.AnyFunSuite
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import scala.util.Random
import scala.Console._
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
  val help =
s"""Test each production.

${RED}****IMPORTANT****${RESET}
${MAGENTA}Note that test case of each production is chosen randomly.
So, if you want to retrive the testcase, copy and paste that in the parse repl.
${RESET}
"""

  println(help)

  val prodMap = Map(
    "Atom" -> atom,
    "Primary" -> primary,
    "AwaitPrimary" -> awaitPrimary,
    "Power" -> power,
    // "Uop" -> uop,
    "Factor" -> factor,
    // "Bop" -> bop,
    "Term" -> term,
    // "Sop" -> uop,
    "Sum" -> sum,
    "ShiftExpr" -> shiftExpr,
    "BitAnd" -> bitAnd,
    "BitXor" -> bitXor,
    "BitOr" -> bitOr,
    // "Cop" -> cop,
    "Comparison" -> comparison,
  )

  val partialTestMap: Map[String, List[TestScript]] = Map(
    "Atom" -> List(
      "1", "1.0", "1j",
      "True", "False",
      "id",
      """"str"""",
      // TODO complex atom production
      // tuple, group, genexp
      // list, listcomp
      // dict, set, dictcomp, setcomp
    ),
    "Primary" -> List(
      LazyBinding("Atom"),
      // TODO complex primary production
    ),
    "AwaitPrimary" -> List(
      LazyBinding("Primary"),
      "await" ~ LazyBinding("Primary"),
    ),
    "Power" -> List(
      LazyBinding("AwaitPrimary"),
      LazyBinding("AwaitPrimary") ~ "**" ~ LazyBinding("Factor"),
    ),
    "Uop" -> List("+", "-", "~"),
    "Factor" -> List(
      LazyBinding("Power"), LazyBinding("Uop") ~ LazyBinding("Power"),
    ),
    "Bop" -> List("*", "/", "//", "%", "@"),
    "Term" -> List(
      LazyBinding("Factor"),
      LazyBinding("Term") ~ LazyBinding("Bop") ~ LazyBinding("Factor"),
    ),
    "Sop" -> List("+", "-"),
    "Sum" -> List(
      LazyBinding("Term"),
      LazyBinding("Sum") ~ LazyBinding("Sop") ~ LazyBinding("Term"),
    ),
    "ShiftExpr" -> List(
      LazyBinding("Sum"),
      LazyBinding("ShiftExpr") ~ ">>" ~ LazyBinding("Sum"),
      LazyBinding("ShiftExpr") ~ "<<" ~ LazyBinding("Sum"),
    ),
    "BitAnd" -> List(
      LazyBinding("ShiftExpr"),
      LazyBinding("BitAnd") ~ "&" ~ LazyBinding("ShiftExpr"),
    ),
    "BitXor" -> List(
      LazyBinding("BitAnd"),
      LazyBinding("BitXor") ~ "^" ~ LazyBinding("BitAnd"),
    ),
    "BitOr" -> List(
      LazyBinding("BitXor"),
      LazyBinding("BitOr") ~ "|" ~ LazyBinding("BitXor"),
    ),
    "Cop" -> List(
      "==", "!=", "<=", ">=", "<", ">", "not" ~ "in", "in", "is" ~ "not", "is"
    ),
    // TODO add rep feature
    "Comparison" -> List()
  )

  // TODO: Add test for invalid syntax
  // val partialSyntaxErrorMap = ???
  def weightedRandomIndex(length: Int) = {
    val n = Random.nextInt(length * 2)
    (n / 2) * (n % 2)
  }

  def toTestString(TestScript: TestScript): List[String] = TestScript match {
    case Normal(test) => List(test)
    // test all possible way
    // case LazyBinding(name) => partialTestMap(name).flatMap(toTestString)
    // test one way in each production randomly
    case LazyBinding(name) => 
      val candidate = partialTestMap(name)
      toTestString(candidate(weightedRandomIndex(candidate.length)))
    case ~(a, b) => 
      for {
        aa <- toTestString(a)
        bb <- toTestString(b)
      } yield s"$aa $bb"
  }

  trait TestScript {
    def ~(rhs: TestScript) = new ~(this, rhs)
    def ~(rhs: String) = new ~(this, rhs)
  }

  case class LazyBinding(name: String) extends TestScript

  case class Normal(test: String) extends TestScript

  case class ~(a: TestScript, b: TestScript) extends TestScript

  implicit def toTestScript(str: String): TestScript = Normal(str)

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

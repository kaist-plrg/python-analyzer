package kr.ac.kaist.pyanalyzer.parser

import scala.util.Random._

object Grammar {

  // TestGenerator

  // TODO: update the random sampling algorithm
  def weightedRandomIndex(length: Int) = {
    val n = nextInt(length * 4)
    if (n >= length) 0 else n
  }

  val repetitions = "0" * 80 + "1" * 15 + "2" * 4 + "3"

  sealed trait TestGenerator {
    def ~(rhs: TestGenerator) = new ~(this, rhs)
    def ~(rhs: String) = new ~(this, rhs)
    // ****Important****
    // each toString call returns a different String
    override def toString: String = this match {
      case Normal(test) => test
      case Prod(name) =>
        val candidate = PEG_Grammar(name)
        candidate(weightedRandomIndex(candidate.length))
      case ~(a, b) => s"$a $b"
      // TODO: update repetition
      // 0 ~ 2 repetition
      case Rep(a) =>
        val times = repetitions.charAt(nextInt(100)) - 48
        (for (i <- 1 to times) yield a).mkString(" ")
    }
  }

  case class Normal(test: String) extends TestGenerator

  case class Prod(name: String) extends TestGenerator

  case class ~(a: TestGenerator, b: TestGenerator) extends TestGenerator

  case class Rep(a: TestGenerator) extends TestGenerator

  def Rep1(a: TestGenerator): TestGenerator = a ~ Rep(a)

  def Opt(t: TestGenerator): TestGenerator =
    if (nextInt(2) == 0) "" else t

  def RepSep1(t: TestGenerator, sep: String): TestGenerator = t ~ Rep1(sep ~ t)

  implicit def toTestGenerator(str: String): TestGenerator = Normal(str)

  implicit def mkTest(t: TestGenerator): String = t.toString

  // PEG Grammar

  // TODO: Add more grammar
  val PEG_Grammar: Map[String, List[TestGenerator]] = Map(
    // TODO: Add Comps
    "List" -> List(
      "[" ~ Opt(Prod("StarNamedExpr")) ~ "]",
    ),
    "Tuple" -> List(
      "(" ~ Opt(Prod("StarNamedExpr") ~ "," ~ Opt(Prod("StarNamedExpr"))) ~ ")",
    ),
    "Set" -> List(
      "{" ~ Prod("StarNamedExpr") ~ "}",
    ),
    "Atom" -> List(
      "1", "1.0", "1j",
      "True", "False",
      "id",
      """"str"""",
      Prod("List"), Prod("Tuple"), Prod("Set"),
      // TODO complex atom production
      // tuple, group, genexp
      // list, listcomp
      // dict, set, dictcomp, setcomp

      // Failed case
      // "...",
    ),
    "Primary" -> List(
      Prod("Atom"),
      // TODO complex primary production
    ),
    "AwaitPrimary" -> List(
      Prod("Primary"),
      "await" ~ Prod("Primary"),
    ),
    "Power" -> List(
      Prod("AwaitPrimary"),
      Prod("AwaitPrimary") ~ "**" ~ Prod("Factor"),
    ),
    "Uop" -> List("+", "-", "~"),
    "Factor" -> List(
      Prod("Power"), Prod("Uop") ~ Prod("Power"),
    ),
    "Bop" -> List("*", "/", "//", "%", "@"),
    "Term" -> List(
      Prod("Factor"),
      Prod("Term") ~ Prod("Bop") ~ Prod("Factor"),
    ),
    "Sumop" -> List("+", "-"),
    "Sum" -> List(
      Prod("Term"),
      Prod("Sum") ~ Prod("Sumop") ~ Prod("Term"),
    ),
    "Shiftop" -> List(">>", "<<"),
    "ShiftExpr" -> List(
      Prod("Sum"),
      Prod("ShiftExpr") ~ Prod("Shiftop") ~ Prod("Sum"),
    ),
    "BitAnd" -> List(
      Prod("ShiftExpr"),
      Prod("BitAnd") ~ "&" ~ Prod("ShiftExpr"),
    ),
    "BitXor" -> List(
      Prod("BitAnd"),
      Prod("BitXor") ~ "^" ~ Prod("BitAnd"),
    ),
    "BitOr" -> List(
      Prod("BitXor"),
      Prod("BitOr") ~ "|" ~ Prod("BitXor"),
    ),
    "Cop" -> List(
      "==", "!=", "<=", ">=", "<", ">", "not" ~ "in", "in", "is" ~ "not", "is"
    ),
    "Comparison" -> List(
      Prod("BitOr"),
      Prod("BitOr") ~ Rep1(Prod("Cop") ~ Prod("BitOr")),
    ),
    "Inversion" -> List(
      Prod("Comparison"),
      "not" ~ Prod("Inversion"),
    ),
    "Conjunction" -> List(
      Prod("Inversion"),
      Prod("Inversion") ~ Rep1("and" ~ Prod("Inversion")),
    ),
    "Disjunction" -> List(
      Prod("Conjunction"),
      Prod("Conjunction") ~ Rep1("or" ~ Prod("Conjunction")),
    ),
    "Expression" -> List(
      Prod("Disjunction"),
      Prod("Disjunction") ~ "if" ~ Prod("Disjunction") ~ "else" ~ Prod("Expression"),
    ),
    "Expressions" -> List(
      Prod("Expression"),
      Prod("Expression") ~ ",",
      Prod("Expression") ~ Rep1("," ~ Prod("Expression")) ~ Opt(","),
    ),
    "NamedExpr" -> List(
      // TODO: implement negative lookahead
      Prod("Expression"),
      Prod("AssignExpr"),
    ),
    "AssignExpr" -> List(
      // TODO: implement ~ operator in grammar spec
      "id" ~ ":=" ~ Prod("Expression"),
    ),
    "StarNamedExpr" -> List(
      Prod("NamedExpr"),
      "*" ~ Prod("BitOr"),
    ),
    "StarNamedExprs" -> List(
      RepSep1(Prod("StarNamedExpr"), ",") ~ Opt(","),
    ),
    "StarExpr" -> List(
      Prod("Expression"),
      "*" ~ Prod("BitOr"),
    ),
    "StarExprs" -> List(
      Prod("StarExpr"),
      Prod("StarExpr") ~ ",",
      Prod("StarExpr") ~ Rep1("," ~ Prod("StarExpr")) ~ Opt(","),
    ),
  )
}

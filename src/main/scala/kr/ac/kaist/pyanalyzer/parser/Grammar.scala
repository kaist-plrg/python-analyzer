package kr.ac.kaist.pyanalyzer.parser

import scala.util.Random._

object Grammar {

  // TestGenerator

  // TODO: update the random sampling algorithm
  def weightedRandomIndex(length: Int) = {
    val n = nextInt(length * 5)
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

  def Rep1Sep(t: TestGenerator, sep: String): TestGenerator = t ~ Rep1(sep ~ t)

  implicit def toTestGenerator(str: String): TestGenerator = Normal(str)

  implicit def mkTest(t: TestGenerator): String = t.toString

  // PEG Grammar

  // TODO: Add more grammar
  val PEG_Grammar: Map[String, List[TestGenerator]] = Map(
    "Group" -> List(
      "(" ~ Prod("NamedExpr") ~ ")",
      "(" ~ Prod("YieldExpr") ~ ")",
    ),
    // Data Structures
    "List" -> List(
      "[" ~ Opt(Prod("StarNamedExpr")) ~ "]",
    ),
    // "Listcomp" -> List(
    //   "[" ~ Prod("NamedExpr") ~ Prod("ForIfClauses") ~ "]",
    // ),
    "Tuple" -> List(
      "(" ~ Opt(Prod("StarNamedExpr") ~ "," ~ Opt(Prod("StarNamedExpr"))) ~ ")",
    ),
    "Set" -> List(
      "{" ~ Prod("StarNamedExpr") ~ "}",
    ),
    // "Setcomp" -> List(
    //   "{" ~ Prod("NamedExpr") ~ Prod("ForIfClauses") ~ "}",
    // ),
    "Dict" -> List(
      "{" ~ Opt(Prod("DoubleStarredKvPairs")) ~ "}",
    ),
    // "Dictcomp" -> List(
    //   "{" ~ Prod("KvPair") ~ Prod("ForIfClauses") ~ "}",
    // ),
    "DoubleStarredKvPairs" -> List(
      Rep1Sep(Prod("DoubleStarredKvPair"), ",") ~ Opt(","),
    ),
    "DoubleStarredKvPair" -> List(
      "**" ~ Prod("BitOr"),
      Prod("KvPair"),
    ),
    "KvPair" -> List(
      Prod("Expression") ~ ":" ~ Prod("Expression")
    ),
    // // Comprehension
    // "ForIfClauses" -> List(
    //   Rep1(Prod("ForIfClause")),
    // ),
    // "ForIfClause" -> List(
    //   Opt("async") ~ "for" ~ Prod("StarTargets") ~ "in" ~ Prod("Disjunction") ~
    //     Opt("if" ~ Prod("Disjunction")),
    // ),
    // "StarTargets" -> List(
    //   // TODO: Add negative lookahead
    //   Prod("StarTarget"),
    //   Prod("StarTarget") ~ Rep("," ~ Prod("StarTarget")) ~ Opt(",")
    // ),
    // "StarTargetsListSeq" -> List(
    //   Rep1Sep(Prod("StarTarget"), ",") ~ Opt(","),
    // ),
    // "StarTargetsTupleSeq" -> List(
    //   Prod("StarTarget") ~ ",",
    //   Prod("StarTarget") ~ Rep1("," ~ Prod("StarTarget")) ~ Opt(","),
    // ),
    // "StarTarget" -> List(
    //   Prod("TargetWithStarAtom"),
    //   // TODO: Add negative lookahead
    //   "*" ~ Prod("StarTarget"),
    // ),
    // // TODO: Add negative lookahead
    // "TargetWithStarAtom" -> List(
    //   Prod("StarAtom"),
    //   Prod("TPrimary") ~ "." ~ "id",
    //   // Prod("TPrimary") ~ "[" ~ Prod("Slices") ~ "]",
    // ),
    // "StarAtom" -> List(
    //   "id",
    //   "(" ~ Prod("TargetWithStarAtom") ~ ")",
    //   "(" ~ Opt(Prod("StarTargetsTupleSeq")) ~ ")",
    //   "[" ~ Opt(Prod("StarTargetsListSeq")) ~ "]",
    // ),
    // // TODO: Add negative lookahead
    // "TPrimary" -> List(
    //   Prod("Atom"),
    //   Prod("TPrimary") ~ "." ~ "id",
    //   // Prod("TPrimary") ~ "[" ~ Prod("Slices") ~ "]",
    //   // genexp
    //   // call
    // // Expression
    // ),
    "Atom" -> List(
      // "1",
      "1.0", "1j",
      "True", "False",
      "id",
      """"str"""",
      Prod("Group"), Prod("List"), Prod("Tuple"), Prod("Set"),
      // TODO complex atom production
      // tuple, group, genexp
      // list, listcomp
      // dict, set, dictcomp, setcomp

      // Failed case
      // "...",
    ),
    "Slice" -> List(
      Prod("NamedExpr"),
      Opt(Prod("Expression")) ~ ":" ~ Opt(Prod("Expression")) ~
        Opt(":" ~ Opt(Prod("Expression"))),
    ),
    "Slices" -> List(
      Prod("Slice"),
      Rep1Sep(Prod("Slice"), ",") ~ Opt(","),
    ),
    "Primary" -> List(
      Prod("Atom"),
      Prod("Primary") ~ "." ~ "id",
      // TODO: Call
      Prod("Primary") ~ "[" ~ Prod("Slices") ~ "]",
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
      "==", "!=", "<=", ">=", "<", ">", "not in", "in", "is not", "is"
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
    "Lambdadef" -> List(),
    "LambdaKwds" -> List(),
    "LambdaParamNoDefault" -> List(),
    "LambdaParamWithDefault" -> List(),
    "LambdaParamMaybeDefault" -> List(),
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
      Rep1Sep(Prod("StarNamedExpr"), ",") ~ Opt(","),
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
    "YieldExpr" -> List(
      "yield from" ~ Prod("Expression"),
      "yield" ~ Opt(Prod("StarExpr")),
    ),
  )
}

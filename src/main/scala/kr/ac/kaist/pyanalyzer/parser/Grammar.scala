package kr.ac.kaist.pyanalyzer.parser

import scala.util.Random._

object Grammar {

  // Grammar

  // TODO: update the random sampling algorithm
  def weightedRandomIndex(length: Int) = {
    val n = nextInt(length * 5)
    if (n >= length) 0 else n
  }

  val repetitions = "0" * 80 + "1" * 15 + "2" * 4 + "3"

  sealed trait Grammar {
    def ~(rhs: Grammar) = new ~(this, rhs)
    def ~(rhs: String) = new ~(this, rhs)
    // ****Important****
    // each toString call returns a different String
    def unfolding: TestGenerator = this match {
      case Str(str) => Normal(str)
      case Prod(name) =>
        val candidate = PEG_Grammar(name)
        candidate(weightedRandomIndex(candidate.length))
      case ~(a, b) =>
        val Seq(l, r) = Seq(a, b)
        l match {
          case Normal("") => r // ignore trivial case
          case l => Seq(l, r)
        }
      // TODO: update repetition
      // 0 ~ 2 repetition
      case Rep(a) =>
        val times = repetitions.charAt(nextInt(100)) - 48
        (for (i <- 1 to times) yield a).foldLeft(Str(""): Grammar)((g, e) => g ~ e)
    }
  }

  case class Prod(name: String) extends Grammar

  case class Rep(a: Grammar) extends Grammar

  case class Str(str: String) extends Grammar

  case class ~(a: Grammar, b: Grammar) extends Grammar

  def Rep1(a: Grammar): Grammar = a ~ Rep(a)

  def Opt(t: Grammar): Grammar =
    if (nextInt(2) == 0) "" else t

  def Rep1Sep(t: Grammar, sep: String): Grammar = t ~ Rep1(sep ~ t)

  sealed trait TestGenerator {
    override def toString = this match {
      case Normal(str) => str
      case Seq(a, b) => s"$a $b"
      case LookaheadSeq(a, g) => s"$a"
    }
  }

  case class Normal(str: String) extends TestGenerator

  case class Seq(a: TestGenerator, b: TestGenerator) extends TestGenerator

  case class LookaheadSeq(a: TestGenerator, guard: TestGenerator) extends TestGenerator

  implicit def toGrammar(str: String): Grammar = Str(str)

  implicit def toTestGenerator(g: Grammar): TestGenerator = g.unfolding

  implicit def mkTest(t: TestGenerator): String = t.toString

  // PEG Grammar

  // TODO: Add more grammar
  val PEG_Grammar: Map[String, List[Grammar]] = Map(
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
      // Add negative lookahead
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
    "Expression" -> List(
      Prod("Disjunction"),
      Prod("Disjunction") ~ "if" ~ Prod("Disjunction") ~ "else" ~ Prod("Expression"),
    ),
    "Lambdef" -> List(
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
    ),
    "LambdaParams" -> List(
      Prod("LambdaSlashNoDefault") ~ Rep(Prod("LambdaParamNoDefault")) ~
        Rep(Prod("LambdaParamWithDefault")) ~ Opt(Prod("LambdaStarEtc")),
      Prod("LambdaSlashWithDefault") ~ Rep(Prod("LambdaParamWithDefault")) ~
        Opt(Prod("LambdaStarEtc")),
      Rep1(Prod("LambdaParamNoDefault")) ~ Rep(Prod("LambdaParamWithDefault")) ~
        Opt(Prod("LambdaStarEtc")),
      Rep1(Prod("LambdaParamWithDefault")) ~ Opt(Prod("LambdaStarEtc")),
      Prod("LambdaStarEtc"),
    ),
    "LambdaSlashNoDefault" -> List(
      Rep1(Prod("LambdaParamNoDefault")) ~ "/",
      Rep1(Prod("LambdaParamNoDefault")) ~ "/" ~ ",",
    ),
    "LambdaSlashWithDefault" -> List(
      Rep(Prod("LambdaParamNoDefault")) ~ Rep1(Prod("LambdaParamWithDefault")) ~ "/",
      Rep(Prod("LambdaParamNoDefault")) ~ Rep1(Prod("LambdaParamWithDefault")) ~ "/" ~ ",",
    ),
    "LambdaStarEtc" -> List(
      Prod("LambdaKwds"),
      "*" ~ Prod("LambdaParamNoDefault") ~ Rep(Prod("LambdaParamMaybeDefault")) ~
        Opt(Prod("LambdaKwds")),
      "*" ~ "," ~ Rep1(Prod("LambdaParamMaybeDefault")) ~ Opt(Prod("LambdaKwds")),
    ),
    "LambdaKwds" -> List(
      "**" ~ Prod("LambdaParamNoDefault"),
    ),
    "LambdaParamNoDefault" -> List(
      "id",
      "id" ~ ",",
    ),
    "LambdaParamWithDefault" -> List(
      "id" ~ Prod("Default"),
      "id" ~ Prod("Default") ~ ",",
    ),
    "LambdaParamMaybeDefault" -> List(
      "id" ~ Opt(Prod("Default")),
      "id" ~ Opt(Prod("Default")) ~ ",",
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

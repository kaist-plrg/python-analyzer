package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import scala.util.Random._

object Grammar {

  // TestGenerator

  // TODO: update the random sampling algorithm
  def weightedRandomIndex(length: Int) = {
    val n = nextInt(length * 9)
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
        if (name == "genId") genId
        else {
          val candidate = PEG_Grammar(name)
          candidate(weightedRandomIndex(candidate.length))
        }
      case ~(a, b) => (s"$a", s"$b") match {
        case ("", str2) => str2
        case (str1, "") => str1
        case (str1, str2) => s"$str1 $str2"
      }
      // TODO: update repetition
      // 0 ~ 2 repetition
      case Rep(a) =>
        val times = repetitions.charAt(nextInt(100)) - 48
        (for (i <- 1 to times) yield a).mkString(" ")
    }

    def testWithDepth(depth: Int): String = this match {
      case Normal(test) => test
      case Prod(name) =>
        if (name == "genId") genId
        else {
          val candidate = PEG_Grammar(name)
          val index = nextInt(candidate.length)
          if (depth == 0 || index == 0) candidate(0).testWithDepth(depth)
          else candidate(index).testWithDepth(depth - 1)
        }
      case ~(a, b) => (a.testWithDepth(depth), b.testWithDepth(depth)) match {
        case ("", str2) => str2
        case (str1, "") => str1
        case (str1, str2) => s"$str1 $str2"
      }
      case Rep(a) =>
        val times = nextInt(4)
        val seq = for (i <- 1 to times) yield a.testWithDepth(depth)
        seq.foldLeft(Normal(""): TestGenerator)((t, e) => t ~ e).testWithDepth(depth)
    }

  }

  case class Normal(test: String) extends TestGenerator

  case class Prod(name: String) extends TestGenerator

  case class ~(a: TestGenerator, b: TestGenerator) extends TestGenerator

  case class Rep(a: TestGenerator) extends TestGenerator

  def Rep1(a: TestGenerator): TestGenerator = a ~ Rep(a)

  def Opt(t: TestGenerator): TestGenerator =
    if (nextInt(5) == 0) "" else t

  def Rep1Sep(t: TestGenerator, sep: String): TestGenerator = t ~ Rep1(sep ~ t)

  implicit def toTestGenerator(str: String): TestGenerator = Normal(str)

  implicit def mkTest(t: TestGenerator): String = t.toString

  def genId: String = {
    val length = nextInt(5)
    def capital = nextInt(2) * 32
    def alpha = (65 + nextInt(26) + capital).toChar
    val res = alpha + (for (i <- 1 to length) yield {
      if (nextInt(2) == 0) (48 + nextInt(10)).toChar
      else alpha
    }).mkString("")
    if (keywords contains res) genId else res
  }

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
    "Listcomp" -> List(
      "[" ~ Prod("NamedExpr") ~ Prod("ForIfClauses") ~ "]",
    ),
    "Tuple" -> List(
      "(" ~ Opt(Prod("StarNamedExpr") ~ "," ~ Opt(Prod("StarNamedExpr"))) ~ ")",
    ),
    "Set" -> List(
      "{" ~ Prod("StarNamedExpr") ~ "}",
    ),
    "Setcomp" -> List(
      "{" ~ Prod("NamedExpr") ~ Prod("ForIfClauses") ~ "}",
    ),
    "Dict" -> List(
      "{" ~ Opt(Prod("DoubleStarredKvPairs")) ~ "}",
    ),
    "Dictcomp" -> List(
      "{" ~ Prod("KvPair") ~ Prod("ForIfClauses") ~ "}",
    ),
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
    // Comprehension
    "ForIfClauses" -> List(
      Rep1(Prod("ForIfClause")),
    ),
    "ForIfClause" -> List(
      Opt("async") ~ "for" ~ Prod("StarTargets") ~ "in" ~ Prod("Disjunction") ~
        Opt("if" ~ Prod("Disjunction")),
    ),
    "StarTargets" -> List(
      // TODO: Add negative lookahead
      Prod("StarTarget"),
      Prod("StarTarget") ~ Rep("," ~ Prod("StarTarget")) ~ Opt(",")
    ),
    "StarTargetsListSeq" -> List(
      Rep1Sep(Prod("StarTarget"), ",") ~ Opt(","),
    ),
    "StarTargetsTupleSeq" -> List(
      Prod("StarTarget") ~ ",",
      Prod("StarTarget") ~ Rep1("," ~ Prod("StarTarget")) ~ Opt(","),
    ),
    "StarTarget" -> List(
      Prod("TargetWithStarAtom"),
      // TODO: Add negative lookahead
      // "*" ~ Prod("StarTarget"),
      "*" ~ Prod("TargetWithStarAtom"),
    ),
    // TODO: Add negative lookahead
    "TargetWithStarAtom" -> List(
      Prod("StarAtom"),
      Prod("TPrimary") ~ "." ~ Prod("genId"),
      // Prod("TPrimary") ~ "[" ~ Prod("Slices") ~ "]",
    ),
    "StarAtom" -> List(
      Prod("genId"),
      "(" ~ Prod("TargetWithStarAtom") ~ ")",
      "(" ~ Opt(Prod("StarTargetsTupleSeq")) ~ ")",
      "[" ~ Opt(Prod("StarTargetsListSeq")) ~ "]",
    ),
    // TODO: Add negative lookahead
    "TPrimary" -> List(
      Prod("Atom"),
      Prod("TPrimary") ~ "." ~ Prod("genId"),
      Prod("TPrimary") ~ "[" ~ Prod("Slices") ~ "]",
      Prod("TPrimary") ~ Prod("Genexp"),
      Prod("TPrimary") ~ "(" ~ Opt(Prod("Arguments")) ~ ")",
    // Expression
    ),
    "Atom" -> List(
      // "1",
      "1.0", "1j",
      "True", "False",
      Prod("genId"),
      """"str"""",
      Prod("Group"), Prod("List"), Prod("Tuple"), Prod("Set"),
      Prod("Dict"), Prod("Listcomp"), Prod("Dictcomp"), Prod("Setcomp"),
      Prod("Genexp")
    ),
    "genTarget" -> List(
      Prod("AssignExpr"), Prod("Expression"),
    ),
    "Genexp" -> List(
      "(" ~ Prod("genTarget") ~ Prod("ForIfClauses") ~ ")"
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
      Prod("Primary") ~ "." ~ Prod("genId"),
      Prod("Primary") ~ Prod("Genexp"),
      Prod("Primary") ~ "(" ~ Prod("Arguments") ~ ")",
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
    "Lambdef" -> List(
      "lambda" ~ Opt(Prod("LambdaParams")) ~ ":" ~ Prod("Expression"),
    ),
    "LambdaParams" -> List(
      Prod("LambdaSlashNoDefault") ~ Rep("," ~ Prod("LambdaParamNoDefault")) ~
        Rep("," ~ Prod("LambdaParamWithDefault")) ~ Opt("," ~ Prod("LambdaStarEtc")),
      Prod("LambdaSlashWithDefault") ~ Rep("," ~ Prod("LambdaParamWithDefault")) ~
        Opt("," ~ Prod("LambdaStarEtc")),
      Rep1Sep(Prod("LambdaParamNoDefault"), ",") ~
        Rep("," ~ Prod("LambdaParamWithDefault")) ~ Opt("," ~ Prod("LambdaStarEtc")),
      Rep1Sep(Prod("LambdaParamWithDefault"), ",") ~ Opt("," ~ Prod("LambdaStarEtc")),
      Prod("LambdaStarEtc"),
    ),
    "LambdaSlashNoDefault" -> List(
      Prod("LambdaParamNoDefault") ~ Rep("," ~ Prod("LambdaParamNoDefault")) ~ "," ~ "/",
    ),
    "LambdaSlashWithDefault" -> List(
      Rep(Prod("LambdaParamNoDefault") ~ ",") ~ Prod("LambdaParamWithDefault") ~
        Rep1("," ~ Prod("LambdaParamWithDefault")) ~ "," ~ "/",
    ),
    "LambdaStarEtc" -> List(
      Prod("LambdaKwds"),
      "*" ~ Prod("LambdaParamNoDefault") ~ Rep("," ~ Prod("LambdaParamMaybeDefault")) ~
        Opt("," ~ Prod("LambdaKwds")),
      "*" ~ Rep1("," ~ Prod("LambdaParamMaybeDefault")) ~
        Opt("," ~ Prod("LambdaKwds")),
    ),
    "LambdaKwds" -> List(
      "**" ~ Prod("LambdaParamNoDefault"),
    ),
    "LambdaParamNoDefault" -> List(
      Prod("genId"),
    ),
    "LambdaParamWithDefault" -> List(
      Prod("genId") ~ Prod("Default"),
    ),
    "LambdaParamMaybeDefault" -> List(
      Prod("genId") ~ Opt(Prod("Default")),
    ),
    "Default" -> List(
      "=" ~ Prod("Expression"),
    ),
    "Expression" -> List(
      Prod("Disjunction"),
      Prod("Disjunction") ~ "if" ~ Prod("Disjunction") ~ "else" ~ Prod("Expression"),
      Prod("Lambdef"),
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
      Prod("genId") ~ ":=" ~ Prod("Expression"),
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
    "Arguments" -> List(
      Prod("Args") ~ Opt(","),
    ),
    "Args" -> List(
      Rep1Sep(Prod("argsTarget"), ",") ~ Opt("," ~ Prod("Kwargs")),
    ),
    "argsTarget" -> List(
      Prod("StarredExpr"), Prod("AssignExpr"), Prod("Expression"),
    ),
    "Kwargs" -> List(
      Rep1Sep(Prod("KwargOrStarred"), ",") ~ "," ~
        Rep1Sep(Prod("KwargOrDoubleStarred"), ","),
      Rep1Sep(Prod("KwargOrStarred"), ","),
      Rep1Sep(Prod("KwargOrDoubleStarred"), ","),
    ),
    "StarredExpr" -> List(
      "*" ~ Prod("Expression"),
    ),
    "KwargOrStarred" -> List(
      Prod("genId") ~ "=" ~ Prod("Expression"),
      Prod("StarredExpr"),
    ),
    "KwargOrDoubleStarred" -> List(
      Prod("genId") ~ "=" ~ Prod("Expression"),
      "**" ~ Prod("Expression"),
    ),
    // Statement
    "PassStmt" -> List("pass"),
    "BreakStmt" -> List("break"),
    "ContinueStmt" -> List("continue"),
    "GlobalStmt" -> List(
      "global" ~ Rep1Sep(Prod("genId"), ", "),
    ),
    "NonlocalStmt" -> List(
      "nonlocal" ~ Rep1Sep(Prod("genId"), ", "),
    ),
    "YieldStmt" -> List(
      Prod("YieldExpr"),
    ),
    "AssertStmt" -> List(
      "assert" ~ Prod("Expression") ~ Opt("," ~ Prod("Expression")),
    ),
    // Patterns
    "patterns" -> List(
      Prod("openSeqPat"),   
    ),
    "pattern" -> List(
      Prod("asPat"),
      Prod("orPat"),
    ),
    "asPat" -> List(
      Prod("orPat") ~ "as" ~ Prod("patCaptureTarget"),
    ),
    "orPat" -> List(
      Rep1Sep(Prod("closedPat"), "|"),
    ),
    "closedPat" -> List(
      Prod("literalPat"),
      Prod("capturePat"),
      Prod("wildcardPat"),
      Prod("valuePat"),
      Prod("groupPat"),
      Prod("seqPat"),
      Prod("mapPat"),
      Prod("classPat"),
    ),
    // TODO negative lookahead on signedNum
    "literalPat" -> List(
      Prod("signedNum"),
      Prod("complexNum"),
      "None",
      "True", "False",
    ),
    "literalExpr" -> List(
      Prod("signedNum"),
      Prod("complexNum"),
      "abcd", "xyzw",
      "None",
      "True", "False",
    ),
    "complexNum" -> List(
      "1.0 + 1.0j" 
    ),
    "signedRealNum" -> List(
      "1.0", "-1.0",
    ),
    "signedNum" -> List(
      "1.0", "-1.0",
    ),
    "realNum" -> List(
      "1.0"  
    ),
    "imagNum" -> List(
      "1.0"
    ),
    "capturePat" -> List(
      Prod("patCaptureTarget") 
    ),
    // TODO negative lookahead
    "patCaptureTarget" -> List(
      Prod("genId")
    ),
    "wildcardPat" -> List(
      "_"
    ),
    // TODO negative lookahead
    "valuePat" -> List(
      Prod("attr")
    ),
    "attr" -> List(
      Prod("nameOrAttr") ~ ("," ~ Prod("genId")),
    ),
    "nameOrAttr" -> List(
      "." ~ Prod("genId"),
      Prod("attr"),
    ),
    "groupPat" -> List(
      "(" ~ Prod("pattern") ~ ")",
    ),
    "seqPat" -> List(
      "[" ~ Opt(Prod("maybeSeqPat")) ~ "]",
      "(" ~ Opt(Prod("openSeqPat")) ~ ")",
    ),
    "openSeqPat" -> List(
      Prod("maybeStarPat") ~ ","  ~ Opt(Prod("maybeSeqPat"))
    ),
    "maybeSeqPat" -> List(
      Rep1Sep(Prod("maybeStarPat"), ",") ~ Opt(",") 
    ),
    "maybeStarPat" -> List(
      Prod("starPat"),
      Prod("pattern"),
    ),
    "starPat" -> List(
      "*" ~ Prod("patCaptureTarget"),
      "*" ~ Prod("wildcardPat"),
    ),
    "mapPat" -> List(
      "{ }",
      "{" ~ Prod("doubleStarPat") ~ Opt(",") ~ "}",
      "{" ~ Prod("itemsPat") ~ "," ~ Prod("doubleStarPat") ~ Opt(",") ~ "}",
      "{" ~ Prod("itemsPat") ~ Opt(",") ~ "}", 
    ),
    "itemsPat" -> List(
      Rep1Sep(Prod("kvPat"), ","),   
    ),
    "kvPat" -> List(
      Prod("literalExpr") ~ ":" ~ Prod("pattern"),
      Prod("attr") ~ ":" ~ Prod("pattern"),
    ),
    "doubleStarPat" -> List(
      "**" ~ Prod("patCaptureTarget")
    ),
    "classPat" -> List(
      Prod("nameOrAttr") ~ "( )",
      Prod("nameOrAttr") ~ "(" ~ Prod("posPats") ~ Opt(",") ~ ")",
      Prod("nameOrAttr") ~ "(" ~ Prod("keywordPats") ~ Opt(",") ~ ")",
      Prod("nameOrAttr") ~ "(" ~ Prod("posPats") ~ "," ~ Prod("keywordPats") ~ Opt(",") ~ ")", 
    ),
    "posPats" -> List(
      Rep1Sep(Prod("pattern"), ",")
    ),
    "keywordPats" -> List(
      Rep1Sep(Prod("keywordPat"), ",") 
    ),
    "keywordPat" -> List(
      Prod("genId") ~ "=" ~ Prod("pattern"),
    ),
  )
}

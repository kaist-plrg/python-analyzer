package kr.ac.kaist.pyanalyzer.parser

import scala.io.Source
import kr.ac.kaist.pyanalyzer.parser.ast._

object SourceParser {
  def readSource(filename: String): String = {
    val source = Source.fromFile(filename)
    val text = try source.mkString finally source.close()
    removeStmt(text)
  }

  def parseText(source: String): List[Token] = {
    IndentParser.parse(source)
  }

  // TODO change Expr to Stm t
  def apply(filename: String): Expr = {
    val source = readSource(filename)
    if (source == "") EEmpty
    else {
      val tokens = parseText(source)
      TokenListParser(tokens) 
    }
  }

  def removeStmt(text: String): String = {
    val lines = text.split("\n")
    // println
    // println("Not Supported texts:")
    // println
    lines.foldLeft("")((s, line) => line match {
      case line if notSupported matches line =>
        // println(line)
        s
      case line => s"$s$line\n"
    })
  }

  val notSupported = List(
    "assert", "async", "await", "break", "class", "continue", "def",
    "del", "elif", "if", "else", "except", "finally", "for",
    "from", "global", "import", "nonlocal", "pass", "raise", "return",
    "try", "while", "with", "yield"
  ).mkString(".*(", "|", ").*").r
}

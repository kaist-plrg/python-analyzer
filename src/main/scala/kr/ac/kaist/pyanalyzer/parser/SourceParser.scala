package kr.ac.kaist.pyanalyzer.parser

import scala.io.Source
import kr.ac.kaist.pyanalyzer.parser.ast._

object SourceParser {
  def readSource(filename: String): String = {
    val source = Source.fromFile(filename)
    val text = try source.mkString finally source.close()
    text
  }

  def parseText(source: String): List[Token] = {
    IndentParser.parse(source)
  }

  def apply(filename: String): List[Stmt] = {
    val source = readSource(filename)
    val tokens = parseText(source)
    val stmts = TokenListParser(tokens) 
    stmts
  }
}

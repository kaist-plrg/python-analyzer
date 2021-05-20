package kr.ac.kaist.pyanalyzer.parser

object SourceParser {
  def apply(source: String): List[Token] = {
    IndentParser.parse(source)
  }
}

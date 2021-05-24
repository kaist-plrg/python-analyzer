package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokensParser {
  def apply(source: String): List[Stmt] = ??? 
}
trait TokensParser extends Parsers {
  type Elem = Token
}

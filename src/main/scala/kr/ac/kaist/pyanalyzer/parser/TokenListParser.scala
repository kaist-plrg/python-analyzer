package kr.ac.kaist.pyanalyzer.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import kr.ac.kaist.pyanalyzer.parser.ast._

object TokenListParser extends TokenListParsers {
  def apply(tokens: List[Token]): List[Stmt] = ??? 
}
trait TokenListParsers extends Parsers {
  type Elem = Token
  case class TokenPosition(column: Int, line: Int, protected val lineContents: String) extends Position
  abstract class TokenReader extends Reader[Token] { outer =>
    val tokens: List[Token]
    val pos: TokenPosition

    def isNewline: Boolean = tokens.head match {
      case Newline => true
      case _ => false
    }

    def width: Int = first.toString.length

    def atEnd: Boolean = tokens.isEmpty
    def first: Token = tokens.head
    def rest: TokenReader = new TokenReader {
      val tokens = outer.tokens.tail
      val pos = if (outer.isNewline) {
        TokenPosition(outer.pos.line + 1, outer.pos.column + outer.width, first.toString)
      } else {
        TokenPosition(outer.pos.line, outer.pos.column + outer.width, first.toString)
      }
    }
  }
  object TokenReader {
    def apply(ts: List[Token]): TokenReader = new TokenReader {
      val tokens = ts
      val stringList = List("") // TODO
      val pos = TokenPosition(1, 1, stringList.head)
    }
  }
}

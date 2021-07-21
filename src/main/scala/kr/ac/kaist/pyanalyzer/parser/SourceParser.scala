package kr.ac.kaist.pyanalyzer.parser

import scala.io.Source
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import java.io._


////////////////////////////////
// SourceParser
// Defines the whole parsing pipeline from file to AST
////////////////////////////////
object SourceParser {
  // read the source code text, given the path to file
   def readSource(filename: String): String = {
     val source = Source.fromFile(filename)
     val text = try source.mkString finally source.close()
     text
   }
  
   // parse the source code text into ast
   def parseSource(source: String): Module = {
     val tokens = Tokenizer.tokenizeText(source)  

     val reader = new PackratReader(TokenListParser.TokenReader(tokens))
     val parser = TokenListParser.module
     val parseResult = parser(reader)

     parser(reader) match {
       case Success(result, rest) => result
       case result => throw new RuntimeException(s"Parsing fail\n$result")
     }
   }

   // given the file path, parse the source code text
   // and returns AST
   def parseFile(filename: String): Module = parseSource(readSource(filename))
}

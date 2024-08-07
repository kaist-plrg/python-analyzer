package kr.ac.kaist.pyanalyzer.parser

import java.io.File
import java.nio.charset.CodingErrorAction
import kr.ac.kaist.pyanalyzer.parser.AstJsonParser
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.io.Codec
import scala.io.Source

////////////////////////////////
// SourceParser
// Defines the whole parsing pipeline from file to AST
////////////////////////////////
object SourceParser {
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  // read the source code text, given the path to file
   def readSource(filename: String): String = {
     val source = Source.fromFile(filename)
     val text = try source.mkString finally source.close()
     text
   }

   def readSource(file: File): String = {
     val source = Source.fromFile(file)
     val text = try source.mkString finally source.close()
     text
   }
  
   // parse the source code text into ast
   def parseSource(source: String, name: String = ""): Module = {
     /*
     val res = AstJsonParser.parseSource(source)
     res.copy(name = name)
     */
     val tokens = Tokenizer.tokenizeText(source)  
     if (tokens == List(EndToken)) return Module()

     val reader = new PackratReader(TokenListParser.TokenReader(tokens))
     val parser = TokenListParser.module
     val parseResult = parser(reader)

     parser(reader) match {
       case Success(result, rest) => result.copy(name = name)
       case result => throw new RuntimeException(s"Parsing fail\n$result")
     }
   }

   // given the file path, parse the source code text
   // and returns AST
   def parseFile(filename: String, name: String = ""): Module =
     parseSource(readSource(filename), name)

   def parseFile(file: File): Module =
     if (!file.getName().endsWith(".py"))
       throw EmptyFileException
     else
       parseSource(readSource(file), file.getName())
}

package kr.ac.kaist.pyanalyzer.parser

import scala.io.Source

object SourceParser {
   def readSource(filename: String): String = {
     val source = Source.fromFile(filename)
     val text = try source.mkString finally source.close()
     text
   }
  
  //  def apply(filename: String): Unit = {
  //    val source = readSource(filename)
  //    if (source == "") ???
  //    else {
  //      val tokens = tokenizeText(source)
  //      println(TokenListParser(tokens))
  //    }
  //  }
}

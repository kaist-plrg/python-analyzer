package kr.ar.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import scala.Console._ 

class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  var epoch = 0
  var setPrompt = true
  def prompt(s: String): Unit = if (setPrompt) { println(s) }

  val srcFiles: List[String] = List(
    s"$PY_SOURCE_DIR/simple/test01/main.py",
    //s"$PY_SOURCE_DIR/simple/parser-regress/main.py",
  )

  def parseSource(t: String) = {
    val tokens = SourceParser.tokenizeText(t)
    println(s"tokenized result:\n$tokens")
    val reader = new PackratReader(TokenReader(tokens))
    val parser = TokenListParser.module
    parser(reader) match {
      case Success(result, rest) if rest.first == NewlineToken => result
      case result => throw new RuntimeException(s"Parsing fail\ntest:\n\n$result")
    }
  }
  
  def testFile(filename: String): Unit = test(s"FileParseTest:$filename"){
    try { 
      prompt("================================")
      prompt(s"${MAGENTA}Test count: $epoch\n${RESET}")

      val text = SourceParser.readSource(filename)
      prompt(s"${CYAN}Source Text:${RESET}\n$text\n")

      prompt("------------------------")
      val ast01 = parseSource(text) 
      prompt(s"${CYAN}First ast:${RESET}\n$ast01\n")
      
      val pretty01: String = beautify(ast01)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty01\n")

      prompt("------------------------")
      val ast02 = parseSource(pretty01)
      prompt(s"${CYAN}Second ast:${RESET}\n$ast02\n")

      val pretty02: String = beautify(ast02)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty02\n")
      
      assert(pretty01 == pretty02)
      prompt("================================")
    } catch { 
      case e => println(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
      fail
    }
    finally {
      epoch += 1
    }
  }

  def init: Unit = {
    println(help)
    
    for (filename <- srcFiles) {
      testFile(filename)
    }
  }

  init
}

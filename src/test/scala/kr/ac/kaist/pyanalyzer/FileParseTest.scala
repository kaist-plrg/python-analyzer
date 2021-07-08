package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import scala.Console._ 

// FileParseTest: iterate over Python source files, test to parse and unparse correctly
// and the filename should be `main.py` (refer to makeFileName)

class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  var epoch = 1
  // `setPrompt = true` to see message
  var setPrompt = false
  def prompt(s: String): Unit = if (setPrompt) { println(s) }

  // parsing routine
  def parseSource(t: String) = {
    val tokens = SourceParser.tokenizeText(t)
    prompt(s"${CYAN}tokenized result:${RESET}\n${Token.coloredTokens(tokens)}")
    val reader = new PackratReader(TokenReader(tokens))
    val parser = TokenListParser.statements
    parser(reader) match {
      case Success(result, rest) => result
      case result => throw new RuntimeException(s"Parsing fail\ntest:\n\n$result")
    }
  }
  
  // main testing routine
  def testFile(testname: String, filename: String): Unit = test(s"FileParseTest [$testname]"){
    try { 
      prompt("================================")
      prompt(s"${MAGENTA}Test count: $epoch\n${RESET}")

      val text = SourceParser.readSource(filename)
      prompt(s"${CYAN}Source Text:${RESET}\n$text\n")

      prompt("----First Parse--------------------")
      val ast01 = parseSource(text) 
      prompt(s"${CYAN}First ast:${RESET}\n$ast01\n")
      
      val pretty01: String = beautify(ast01)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty01\n")

      prompt("----Second Parse--------------------")
      val ast02 = parseSource(pretty01)
      prompt(s"${CYAN}Second ast:${RESET}\n$ast02\n")

      val pretty02: String = beautify(ast02)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty02\n")
      
      assert(pretty01 == pretty02)
      prompt("================================")
    } catch { 
      case e => 
        throw e
        println(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
        fail
    }
    finally {
      epoch += 1
    }
  }

  def testFileSet(set: FileTestSet): Unit = {
    for {
      (testname, filepath) <- set.filePaths
    } testFile(testname, filepath)
  }

  // run parsing test for each test sets
  val testSetList: List[FileTestSet] = List(
    Microbenchmark   
  )
  def init: Unit = {
    prompt(help) 
    for (set <- testSetList) testFileSet(set)
  }

  init
}

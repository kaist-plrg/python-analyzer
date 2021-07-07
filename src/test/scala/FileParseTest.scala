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

// FileParseTest: iterate over Python source files, test to parse and unparse correctly
// files should locate in $PY_SOURCE_DIR, and subdirectories must be category and name
// and the filename should be `main.py` (refer to makeFileName)

class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  var epoch = 1
  // `setPrompt = true` to see message
  var setPrompt = true
  def prompt(s: String): Unit = if (setPrompt) { println(s) }

  // locating python source file
  def makeFileName(cat: String, name: String): String = s"$PY_SOURCE_DIR/$cat/$name/main.py"

  // parsing routine
  def parseSource(t: String) = {
    val tokens = SourceParser.tokenizeText(t)
    println(s"${CYAN}tokenized result:${RESET}\n$tokens")
    val reader = new PackratReader(TokenReader(tokens))
    val parser = TokenListParser.statements
    parser(reader) match {
      case Success(result, rest) => result
      case result => throw new RuntimeException(s"Parsing fail\ntest:\n\n$result")
    }
  }
  
  // main testing routine
  def testFile(cat: String, name: String): Unit = test(s"FileParseTest [$cat:$name]"){
    val filename = makeFileName(cat, name)
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
      case e => 
        throw e
        println(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
        fail
    }
    finally {
      epoch += 1
    }
  }

  // test targets : list of (category_name, test_name list)
  // refer to src/main/resources/py-source
  val targets: List[(String, List[String])] = List(
    ("simple", List("test01")),
    //("returns", List("call", "return_complex")), 
  )

  def init: Unit = {
    prompt(help)
    
    for {
      (cat, names) <- targets
      name <- names
    } testFile(cat, name)
  }

  init
}

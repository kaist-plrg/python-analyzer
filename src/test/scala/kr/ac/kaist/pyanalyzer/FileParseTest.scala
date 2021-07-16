package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Appender
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import scala.Console._ 
import java.io._

// FileParseTest: iterate over Python source files, test to parse and unparse correctly
// and the filename should be `main.py` (refer to makeFileName)
final case object EmptyFileException extends Exception("Empty File")
class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  val logPath = s"$TEST_LOG_DIR/FileParseTest"
  val logWriter = getPrintWriter(logPath)

  var epoch = 1
  // `setPrompt = true` to see message
  var setPrompt = false
  def prompt(s: String): Unit = {
    if (setPrompt) println(s)
    logWriter.write(withoutColorCodes(s) + "\n")
  }

  // parsing routine
  def parseSource(t: String, checkEmpty: Boolean = false) = {
    val tokens = Tokenizer.tokenizeText(t)
    //prompt(s"${CYAN}tokenized raw:${RESET}\n${tokens}")
    //prompt(s"${CYAN}tokenized result:${RESET}\n${Token.coloredTokens(tokens)}")
    
    if (checkEmpty && tokens.isEmpty) throw EmptyFileException

    val reader = new PackratReader(TokenListParser.TokenReader(tokens))
    val parser = TokenListParser.module
    val parseResult = parser(reader)

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
      prompt(s"${MAGENTA}Test name: $testname\n${RESET}")

      val text = SourceParser.readSource(filename)
      prompt(s"${CYAN}Source Text:${RESET}\n$text\n")

      prompt("----First Parse--------------------")
      val ast01 = parseSource(text, true) 
      //prompt(s"${CYAN}First ast:${RESET}\n$ast01\n")
      
      val pretty01: String = beautify(ast01)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty01\n")

      prompt("----Second Parse--------------------")
      val ast02 = parseSource(pretty01)
      //prompt(s"${CYAN}Second ast:${RESET}\n$ast02\n")

      val pretty02: String = beautify(ast02)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty02\n")
      
      assert(pretty01 == pretty02)
      prompt("================================")
    } catch { 
      case EmptyFileException =>
        prompt(s"${MAGENTA}Epoch $epoch: Empty File${RESET}\n\n")
        cancel 
      case e => 
        throw e
        prompt(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
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
    Microbenchmark,
    HorovodTest
  )

  def init: Unit = {
    prompt(help) 

    for (set <- testSetList) testFileSet(set)

    logWriter.flush()
  }

  init
}

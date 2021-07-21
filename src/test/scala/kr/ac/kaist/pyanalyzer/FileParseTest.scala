package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Appender
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import scala.Console._ 
import java.io._

// FileParseTest: iterate over Python source files, test to parse and unparse correctly
// and the filename should be `main.py` (refer to makeFileName)
class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  var epoch = 1

  // log related
  val logPath = s"$TEST_LOG_DIR/FileParseTest"
  implicit val logWriter = getPrintWriter(logPath)
  implicit var setPrompt = false
  
  // main testing routine
  def testFile(testname: String, filename: String): Unit = test(s"FileParseTest [$testname]"){
    try { 
      prompt("================================")
      prompt(s"${MAGENTA}Test count: $epoch\n${RESET}")
      prompt(s"${MAGENTA}Test name: $testname\n${RESET}")

      val text = SourceParser.readSource(filename)
      prompt(s"${CYAN}Source Text:${RESET}\n$text\n")

      prompt("----First Parse--------------------")
      val ast01 = SourceParser.parseSource(text) 
      //prompt(s"${CYAN}First ast:${RESET}\n$ast01\n")
      
      val pretty01: String = beautify(ast01)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty01\n")

      prompt("----Second Parse--------------------")
      val ast02 = SourceParser.parseSource(pretty01)
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
    HorovodTests
  )

  def init: Unit = {
    prompt(help) 

    for (set <- testSetList) testFileSet(set)

    logWriter.flush()
  }

  init
}

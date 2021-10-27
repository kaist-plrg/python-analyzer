package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.TFlowHvdPairs._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.Console._

class FileTransformTest extends AnyFunSuite {
  val help = """Test tranformation between original and horovod source codes."""
  var epoch = 1

  // log related
  val logPath = s"$TEST_LOG_DIR/FileTransformTest"
  implicit val logWriter = getPrintWriter(logPath)
  implicit val setPrompt = false

  def testFilePair(testname: String, pathPair: TransformPair): Unit =
    test(s"FileTransformTest [$testname]"){
      try {
        prompt("============================")
        prompt(s"${MAGENTA}Test count: $epoch\n${RESET}")
        prompt(s"${MAGENTA}Test name: $testname\n${RESET}")
        val TransformPair(orgPath, ansPath) = pathPair

        // parse thee original source and answer source
        val orgAst = parseFile(orgPath)
        val ansAst = parseFile(ansPath)

        // transform the orgAst
        val transAst = Transformer(orgAst)

        // compare by prettyprint
        val transCode = beautify(transAst)
        prompt(colored(s"${CYAN}")("----------------------------"))
        prompt(s"${CYAN}Transformed Code:${RESET}\n")
        prompt(colored(s"${CYAN}")("----------------------------"))
        prompt(s"$transCode")
        val ansCode = beautify(ansAst)
        prompt(colored(s"${CYAN}")("----------------------------"))
        prompt(s"${CYAN}Answer Code:${RESET}\n")
        prompt(s"$ansCode")
        prompt(colored(s"${CYAN}")("----------------------------"))

        // assert
        assert(ansCode == transCode) 
        prompt("============================")
      } catch {
        case EmptyFileException =>
          prompt(s"${MAGENTA}Epoch $epoch: Empty File${RESET}\n\n")
          cancel
        case e: Exception =>
          prompt(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
          fail
      } finally{
        epoch += 1
      }
    }

  def testFilePairs(set: FileTransformPairs): Unit = {
    for ((testname, transPair) <- set.transformPairs) 
      testFilePair(testname, transPair)
  }

  val testSets: List[FileTransformPairs] = List(
    TFlowHvdPairs, 
  )

  def init: Unit = { 
    prompt(help)
    for (set <- testSets) testFilePairs(set)
    flush()
  }

  init
}

package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._

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
        val transAst = Transfomer(orgAst)
        // compare by prettyprint
        val transCode = beautify(transAst)
        val ansCode = beautify(ansAst)
        // assert
        assert(ansCode == transCode)       
        prompt("============================")
      } catch {
        case EmptyFileException =>
          prompt(s"${MAGENTA}Epoch $epoch: Empty File${RESET}\n\n")
          cancel
        case e: Exception =>
          prompt(s"${MAGNETA}Epoch $epoch failed:${RESET}\n$e\n")
          fail
      } finally{
        epoch += 1
      }
    }

  val files = walkTree(HOROVOD_DIR)

  for {
    file <- files
    path = file.getPath
    if path endsWith ".py"
    relPath = path.drop(HOROVOD_DIR.length + 1)
    if relPath contains "/org/"
  } test(relPath) {
    val hvdPath = HOROVOD_DIR + "/" + relPath.replace("org", "hvd")
    testFilePair(path, hvdPath)
  }
  
  def testFilePair(givenPath: String, ansPath: String): Unit = {
    val givenAst = parseFile(givenPath)
    if (givenAst.body.isEmpty) cancel
    else {
      val transformedAst = Transformer(givenAst)
      val transformedCode = beautify(transformedAst)
      val ansAst = parseFile(ansPath)
      val ansCode = beautify(ansAst)
      val success = ansCode == transformedCode
      assert(success)
    }
  }

  def testFilePairs(set: FileTransformPairs): Unit = {
    for ((testname, transPair)) <- set.transformPairs) 
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

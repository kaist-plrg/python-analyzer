package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._

class FileTransformTest extends AnyFunSuite {
  val help = """Test tranformation between original and horovod source codes."""
  val files = walkTree(HOROVOD_DIR)

  for
  {
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
}

package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.util.Useful._
import java.io._

object HorovodTest extends HorovodTests
class HorovodTests extends FileTestSet {
  val rootPath: String = s"$HOROVOD_DIR" 

  // test targets: all `*.py` files in the directory 
  def targets: List[String] = walkTree(rootPath).toList.map(f => f.getPath()).filter(s => s.endsWith(".py"))
  def makeTestName(path: String): String = path.slice(path.lastIndexOf("tensorflow-to-horovod"), path.length)
  
  def filePaths: Iterator[(String, String)] = 
    for {
      path <- targets.iterator
    } yield (makeTestName(path), path)
}

package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.{ Info, DirInfo }
import kr.ac.kaist.pyanalyzer.util.DirWalker._
import java.io.File

case object ParsePipe extends Pipeline[File, Info[Module]] {
  def execute(path: File): Info[Module] = {
    val relPath = path.getPath() diff HOROVOD_DIR
    if (path.isFile()) {
      walkFile(path.getParentFile())(parseFile(_)) match {
        case dInfo: DirInfo[Module] =>
          val name = relPath.slice(0, relPath indexOf "/org/")
          dInfo.copy(dirname=name)
        case _ => ???
      }
    } else {
      walkFile(path)(parseFile(_)) match {
        case dInfo: DirInfo[Module] => dInfo.copy(dirname=relPath)
        case _ => ???
      }
    }
  }
}

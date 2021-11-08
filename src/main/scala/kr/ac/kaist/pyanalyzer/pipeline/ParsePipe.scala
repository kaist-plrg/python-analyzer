package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.pipeline.Pipeline
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Info
import kr.ac.kaist.pyanalyzer.util.DirWalker._
import java.io.File

case object ParsePipe extends Pipeline[File, Info[Module]] {
  def execute(path: File): Info[Module] = 
    if (path.isFile()) {
      walkFile(path.getParentFile())(parseFile(_))
    } else {
      walkFile(path)(parseFile(_))
    }
}

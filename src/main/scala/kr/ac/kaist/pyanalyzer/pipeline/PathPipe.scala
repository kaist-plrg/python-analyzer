package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File
import kr.ac.kaist.pyanalyzer.util.{ Info, DirInfo }
import kr.ac.kaist.pyanalyzer.util.DirWalker._

case object PathPipe extends Pipeline[String, Info[File]] {
  def execute(s: String): Info[File] = {
    val file = new File(s)
    val dir = if (file.isFile()) file.getParentFile() else file
    flatWalkFile(dir)(f => Option.when(f.getName endsWith ".py")(f))
  }
}

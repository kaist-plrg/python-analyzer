package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File

case object PathPipe extends Pipeline[String, File] {
  def execute(s: String): File = new File(s)
}

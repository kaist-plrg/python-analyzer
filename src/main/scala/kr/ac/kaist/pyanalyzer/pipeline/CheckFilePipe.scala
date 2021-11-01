package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File

case object CheckFilePipe extends Pipeline[File, Option[String]] {
  def execute(s: File): Option[String] = Option.when(s.isFile())(s.getName) 
}

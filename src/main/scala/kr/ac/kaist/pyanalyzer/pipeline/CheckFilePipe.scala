package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File

case object CheckFilePipe extends Pipeline[String, Option[String]] {
  def execute(path: String): Option[String] = {
    val file = new File(path)
    Option.when(file.isFile())(file.getName()) 
  }
}

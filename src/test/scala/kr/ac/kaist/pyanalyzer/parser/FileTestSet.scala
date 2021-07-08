package kr.ac.kaist.pyanalyzer.parser

trait FileTestSet {
  // Iterator of (testname, filepath)
  def filePaths: Iterator[(String, String)]
}

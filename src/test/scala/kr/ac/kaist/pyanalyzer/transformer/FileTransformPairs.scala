package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._

trait FileTransformPairs {
  // Iterator of (testname, (original file, transformed file))
  case class TransformPair(org: String, trans: String)
  def transformPairs: Iterator[(String, TransformPair)]
}

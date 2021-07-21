package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._

case class TransformPair(org: String, trans: String)

trait FileTransformPairs {
  // Iterator of (testname, (original file, transformed file))
  def transformPairs: Iterator[(String, TransformPair)]
}

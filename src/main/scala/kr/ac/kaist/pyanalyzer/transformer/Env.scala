package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

case class Env(map: Map[String, Id] = Map())

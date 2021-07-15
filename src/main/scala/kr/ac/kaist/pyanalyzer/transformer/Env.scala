package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

case class Env(map: Map[String, Id] = Map()) {
  def get(s: String): Option[Id] = map.get(s)
  def add(s: String, x: Id): Env = Env(map + (s -> x))
}

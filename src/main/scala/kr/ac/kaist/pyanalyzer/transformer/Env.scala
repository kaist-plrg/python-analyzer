package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

case class Env(private val map: Map[String, Id] = Map()) {
  def getMap: Map[String, Id] = map
  def get(s: String): Option[Id] = map.get(s)
  def add(s: String, x: Id): Env = Env(map + (s -> x))
  def size: Int = map.size
  def \(env: Env): Env = Env(map -- env.getMap.keySet)
}

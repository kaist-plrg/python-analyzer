package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import kr.ac.kaist.pyanalyzer.util.MultiMap._

case class Env(
  private val map: Map[String, Id] = Map(),
  private val classOrder: ClassOrder = 
    ClassOrder()
      .addNode( // list of nodes
        List(
          "tensorflow.optimizers.Adam",
          "tensorflow.keras.optimizers.Adam",
          "tensorflow.keras.models.Sequential",
          "tensorflow.keras.Model",
          ).map(parseStrFullname(_)))
      .addEdge( // list of subclass pairs (child, parent)
        List(
          ("tensorflow.keras.models.Sequential", "tensorflow.keras.Model"),
        ).map((p: (String, String)) => (parseStrFullname(p._1), parseStrFullname(p._2))))
){
  // map
  def getMap: Map[String, Id] = map
  def get(s: String): Option[Id] = map.get(s)
  def add(s: String, x: Id): Env = this.copy(map=map + (s -> x))
  def size: Int = map.size
  def \(env: Env): Env = this.copy(map=map -- env.getMap.keySet)

  // class order
  def getClassOrder: ClassOrder = classOrder
  def contains(fname: Fullname): Boolean = classOrder.nodes contains fname
}

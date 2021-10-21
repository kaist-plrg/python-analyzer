package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._

case class Env(
  private val map: Map[String, Id] = Map(),
  private val classOrder: ClassOrder = ClassOrder(List(
    Classnode(Fullname(List("tensorflow", "optimizers", "Adam"))),
    Classnode(Fullname(List("tensorflow", "keras", "optimizers", "Adam"))),
    Classnode(
      Fullname(List("tensorflow", "keras", "models", "Sequential")),
      Some(Fullname(List("tensorflow", "keras", "Model")))
    ),
    Classnode((Fullname(List("tensorflow", "keras", "Model")))),
  ), Map())
){
  // map
  def getMap: Map[String, Id] = map
  def get(s: String): Option[Id] = map.get(s)
  def add(s: String, x: Id): Env = this.copy(map=map + (s -> x))
  def size: Int = map.size
  def \(env: Env): Env = this.copy(map=map -- env.getMap.keySet)

  // class order
  def getClassOrder: ClassOrder = classOrder
  def contains(cnode: Classnode): Boolean = classOrder.nodes contains cnode
  def contains(fname: Fullname): Boolean = classOrder.nodes exists (_.name == fname)
}

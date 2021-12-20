package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.hierarchy.Fullname

case class Env(
  private val map: Map[String, Id] = Map(),
  private val classOrder: ClassOrder = GIVEN_CLASS_ORDER
) {
  // map
  def getMap: Map[String, Id] = map
  def get(s: String): Option[Id] = map.get(s)
  def apply(s: String): Id = map(s)
  def add(s: String, x: Id): Env = this.copy(map=map + (s -> x))
  def size: Int = map.size
  def \(env: Env): Env = this.copy(map=map -- env.getMap.keySet)
  def contains(key: String): Boolean = map contains key

  // class order
  def getClassOrder: ClassOrder = classOrder
  def contains(fname: Fullname): Boolean = classOrder.nodes contains fname
  def isSubclass(expr: Expr, parentCandidates: List[String]): Boolean =
    parentCandidates.exists(isSubclass(expr, _))
  def isSubclass(expr: Expr, parentName: String): Boolean =
    classOrder.parseFullname(expr) match {
      case Some(fullname) =>
        classOrder.safeIsSubclass(fullname, parseStrFullname(parentName))
      case None => false
    }
}

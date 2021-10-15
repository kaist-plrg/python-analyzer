package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

// node representing module or class
// edge : child -> parent
// no multiple inheritance => only 1 parent per child
case class ClassNode(name: String, parent: Option[ClassNode]) 

case class ClassGraph(nodes: List[ClassNode], vars: Map[String, ClassNode]) {
  // addNode
  def addNode(node: ClassNode): ClassGraph =
    ClassGraph(nodes :+ node, vars)

  def addNode(nodeName: String): ClassGraph =
    ClassGraph(nodes :+ ClassNode(nodeName, None), vars)

  def addNode(nodeName: String, parent: ClassNode): ClassGraph =
    ClassGraph(nodes :+ ClassNode(nodeName, Some(parent)), vars)

  def addNode(nodeName: String, pOpt: Option[ClassNode]): ClassGraph =
    pOpt match {
      case None => addNode(nodeName)
      case Some(parent) => addNode(nodeName, parent)
    }
 
  // addVar
  def addVar(varName: String, node: ClassNode) =
    ClassGraph(nodes, vars + (varName -> node))

  // ischild: checks if targetNode has has a child with name = id
  // returns the child node if found, else None
  def isChild(targetNode: ClassNode, id: String): Option[ClassNode] =
    nodes.filter(node => node.name == id && node.parent == Some(targetNode)).headOption

  // getPath
  def getPath(expr: Expr): (List[String], ClassGraph, ClassNode) = expr match {
    // expr.id case
    case Attribute(expr, id) => {
      val (l, newGraph, pnode) = getPath(expr)
      isChild(pnode, id.name) match {
        case Some(cnode) => (l :+ id.name, newGraph, cnode) 
        case None => {
          val cnode = ClassNode(id.name, Some(pnode))
          (l :+ id.name, newGraph.addNode(cnode), cnode)
        }
      }
    }
    // id case
    case EName(id) => {
      (vars get id.name) match {
        case Some(node) => (List(id.name), this, node) 
        case None => {
          val node = ClassNode(id.name, None)
          (List(id.name), this.addNode(node), node)
        }
      }
    }
    // others: should not appear
    case _ => ???
  }
} 

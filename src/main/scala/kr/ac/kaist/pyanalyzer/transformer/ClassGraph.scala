package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Useful.{ accUntil }

// node representing module or class
// edge : child -> parent
// no multiple inheritance => only 1 parent per child
case class ClassNode(name: String) 

class ClassGraph {
  // representing graph of ClasNodes
  var nodes = List[ClassNode]()
  var edges = Map[Int,Int]() // (child -> parent)

  def addNode(clsname: String): Unit = { nodes = nodes :+ ClassNode(clsname) } 
  def addEdge(from: Int, to: Int): Unit = { edges = edges + (from -> to) }

  def existsName(name: String): Boolean = nodes contains ClassNode(name)
  def getId(node: ClassNode): Int = nodes.indexOf(node)
  def getNode(id: Int) = nodes(id)
  
  def getParentId(id: Int): Option[Int] = edges get id
  def getParentIds(id: Int): List[Int] = accUntil(id)(getParentId(_))

  def addAccessPath(path: List[String]): Unit =
    path.foreach(s => { if (!existsName(s)) addNode(s) })

  // representing map from variables to ClassNode
  var varMap = Map[String, Int]()

  def addVar(name: String, id: Int): Unit = { varMap = varMap + (name -> id) }

  def addModuleImport(alias: Alias): Unit = alias match { 
    case Alias(name, asName) =>
      addAccessPath(alias.name.map(id => id.name)) 
      addVar(
        asName.getOrElse(alias.name.last).name,  // asName or last Id of name
        getId(ClassNode(name.last.name))) // index of node of name.last
  }

  def procImport(stmt: ImportStmt): Unit = stmt match {
    case ImportStmt(as) => as.foreach(addModuleImport)
  }

  def attrFlatten(attr: Expr): (Expr, List[String]) = ??? 

  def procAssign(stmt: AssignStmt): Unit = stmt match {
    case AssignStmt(List(EName(x)), expr, _) => attrFlatten(expr) match {
      case (EName(v), l) =>
        val node = getNode(varMap(v.name))
        // TODO check the l: List[access_path_tokens] is valid and get corresponding node
        // TODO : addVar(x, corresponding node id)
    }
  }
} 

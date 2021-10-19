package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

case object ParentNotFound extends Exception
case object ClassNotFound extends Exception
case object MultiInheritance extends Exception

case class Fullname(names: List[String]) {
  def add(s: String) = Fullname(names :+ s)
  override def toString() = names.mkString(".")
}
case class Classnode(name: Fullname, parent: Option[Classnode])

case class ClassOrder (nodes: Map[Fullname, Classnode], aliases: Map[String, Fullname]) {
  def addNode(name: Fullname, node: Classnode) = this.copy(nodes = nodes + (name -> node))
  def addAlias(a: String, fn: Fullname) = this.copy(aliases = aliases + (a -> fn))

  def parseAccessPath(expr: Expr): Option[Fullname] = expr match {
    case EName(Id(name)) => aliases.get(name)
    case Attribute(e, Id(field)) => parseAccessPath(e).map(_.add(field))
    case _ => None
  }

  def addSubclass(clsname: String, parent: Expr): ClassOrder = 
    addSubclass(Fullname(List(clsname)), parent)

  def addSubclass(clsname: Fullname, parent: Expr): ClassOrder = 
    // get full name of parent class
    parseAccessPath(parent) match {
      case None => throw ParentNotFound 
      // get parent node and add child node with pointing the parent node
      case Some(fname) => nodes.get(fname) match {
        case Some(pnode) => addNode(clsname, Classnode(clsname, Some(pnode)))
        // if parent node not exist, add one before adding child node
        case None => {
          val parentNode = Classnode(fname, None)
          val parentAdded = addNode(fname, parentNode)
          val childNode = Classnode(clsname, Some(parentNode))
          parentAdded.addNode(clsname, childNode) 
        }}}

  def isSubclass(cname: Fullname, targetParentName: Fullname): Boolean = 
    nodes.get(cname) match {
      case None => throw ClassNotFound
      case Some(cnode) => cnode.parent match {
        case None => false 
        case Some(pnode) =>
          if (pnode.name == targetParentName) true
          else isSubclass(pnode.name, targetParentName)
      }
    }
}

object ClassOrder {
  def transferAlias(order: ClassOrder)(a: Alias): ClassOrder = a match {
    case Alias(ns, None) => order
    case Alias(ns, Some(Id(asName))) => {
      val fname = Fullname(ns.map(_.name)) 
      order.addAlias(asName, fname)
    }
  }

  def transferStmt(order: ClassOrder)(stmt: Stmt): ClassOrder =
    stmt match {
      // ImportStmt: add alias relation
      case ImportStmt(as) => as.foldLeft(order)((o: ClassOrder, x: Alias) =>
        transferAlias(o)(x)) 

      // ClassDef: adds corresponding class node to order
      case ClassDef(_, Id(name), pexprs, _, _) => pexprs match {
        // no parent case
        case Nil => {
          val fname = Fullname(List(name))
          val newnode = Classnode(fname, None)
          order.addNode(fname, newnode)
        }
        // one parent case 
        case List(e) => order.addSubclass(name, e)
        // multiple parent case
        case _ => throw MultiInheritance
      }

      // others: nothing
      case _ => order
    }
}

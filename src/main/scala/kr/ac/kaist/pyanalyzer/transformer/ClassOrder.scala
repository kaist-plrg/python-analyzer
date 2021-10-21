package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.MultiMap._

case object ParentNotParsed extends Exception
case class ClassNotFound(s: String) extends Exception(s)
case object MultiInheritance extends Exception

case class Fullname(names: List[String]) {
  def add(s: String) = Fullname(names :+ s)
  def append(f: Fullname) = Fullname(names ++ f.names)
  def prefixed(s: String) = Fullname(s +: names)
  override def toString() = names.mkString(".")
}


case class ClassOrder(
  edges: MultiMap[Fullname, Fullname] = Map(), 
  aliases: Map[String, Fullname] = Map(),
){
  override def toString() = {
    val nodesStr = edges.keys.map(kname => {
      s"$kname <: [${edges(kname).map(_.toString).mkString(", ")}]"
    }).mkString("\n")
    val aliasStr = aliases.toString
    s"Nodes:\n$nodesStr\nAliases:\n$aliasStr\n"
  }

  def nodes: Set[Fullname] = edges.keys.toSet

  def addNode(node: Fullname): ClassOrder = this.copy(edges = edges.addKey(node))
  def addNode(nodes: List[Fullname]): ClassOrder =
    nodes.foldLeft(this)((o: ClassOrder, n: Fullname) => o.addNode(n))

  def addEdge(from: Fullname, to: Fullname): ClassOrder =
    this.copy(edges = edges.addOne(from, to))
  def addEdge(ftl: List[(Fullname, Fullname)]): ClassOrder =
    ftl.foldLeft(this)((o: ClassOrder, p: (Fullname, Fullname)) => o.addEdge(p._1, p._2))

  def addAlias(a: String, fn: Fullname) = this.copy(aliases = aliases + (a -> fn))
 
  def parseFullname(expr: Expr): Option[Fullname] = expr match {
    case EName(Id(name)) => aliases.get(name) match {
      case Some(fname) => Some(fname)
      case None => Some(Fullname(List(name)))
    }
    case Attribute(e, Id(field)) => parseFullname(e).map(_.add(field))
    case _ => None
  }

  def addSubclass(clsname: String, parent: Expr): ClassOrder = 
    addSubclass(Fullname(List(clsname)), parent)

  def addSubclass(clsname: Fullname, parent: Expr): ClassOrder = 
    // get full name of parent class
    parseFullname(parent) match {
      case None => throw ParentNotParsed 
      // get parent node and add child node with pointing the parent node
      case Some(pname) => this.addNode(pname).addEdge(clsname, pname)
    }

  def isSubclass(cname: Fullname, targetParentName: Fullname): Boolean =
    if (cname == targetParentName) true else {
      edges.get(cname) match {
        case None => throw ClassNotFound(cname.toString)
        case Some(pset) =>
          (pset.filter(_ == targetParentName).size >= 1 || 
            pset.map(isSubclass(_, targetParentName))
                .fold(false)((x: Boolean, y: Boolean) =>x || y) )
      }
    }
}

object ClassOrder {
  def transferAlias(order: ClassOrder)(a: Alias): ClassOrder = a match {
    // TODO
    case Alias(ns, None) => order
    case Alias(ns, Some(Id(asName))) => {
      val fname = Fullname(ns.map(_.name)) 
      order.addAlias(asName, fname)
    }
  }

  def transferAliasPrefixed(order: ClassOrder)(prefix: Fullname, a: Alias): ClassOrder =
    a match {
      case Alias(ns, None) => {
        val fname = prefix.append(Fullname(ns.map(_.name))) 
        val asName = ns.map(_.name).mkString(".")
        order.addAlias(asName, fname)
      }
      case Alias(ns, Some(Id(asName))) => {
        val fname = prefix.append(Fullname(ns.map(_.name)))
        order.addAlias(asName, fname)
      }
    }

  def transferStmt(order: ClassOrder)(stmt: Stmt): ClassOrder =
    stmt match {
      // ImportStmt: add alias relation
      case ImportStmt(as) => as.foldLeft(order)((o: ClassOrder, x: Alias) =>
        transferAlias(o)(x)) 

      // FromImport: add alias with prefix (ignores level)
      case ImportFromStmt(_, fromId, as) => {
        val prefix = Fullname(fromId.map(_.name)) 
        as.foldLeft(order)((o: ClassOrder, x: Alias) => transferAliasPrefixed(o)(prefix, x))
      }

      // ClassDef: adds corresponding class node to order
      case ClassDef(_, Id(name), pexprs, _, _) => pexprs match {
        // no parent case
        case Nil => {
          val cname = Fullname(List(name))
          order.addNode(cname)
        }
        // one parent case 
        case List(e) => order.addSubclass(name, e)
        // multiple parent case
        case l => l.foldLeft(order)((o :ClassOrder, e: Expr) => {
          order.addSubclass(name, e)
        }) 
      }

      // others: nothing
      case _ => order
    }

  
  def parseStrFullname(s: String): Fullname = Fullname(s.split(".").toList)
}

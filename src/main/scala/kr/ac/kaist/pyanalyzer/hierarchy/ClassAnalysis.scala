package kr.ac.kaist.pyanalyzer.hierarchy

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.DirWalker._
import kr.ac.kaist.pyanalyzer.util.{ Info, DirInfo, FileInfo }
import kr.ac.kaist.pyanalyzer.util.MultiMap._
import kr.ac.kaist.pyanalyzer.util.Errors.EmptyFileException
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.hierarchy.Fullname
import kr.ac.kaist.pyanalyzer.transformer._
import java.io.File
import scala.io.Source._

object ClassAnalysis {
  // implicit class for joining multimap with module name prefixed
  implicit class NameOrder(mmap: MultiMap[Fullname, Fullname]) {
    // prefix: prefix to add
    // nochange: Fullname that start with (s \in nochange) will not changed  
    // ex. nochange = Set("tensorflow", "numpy")
    def addPrefix(prefix: String, nochange: Set[String]): MultiMap[Fullname, Fullname] =
      mmap.keys.map(kname => {
        val nkey: Fullname = 
          if (nochange contains kname.names(0)) { kname }
          else { kname.prefixed(prefix) }
        val nset: Set[Fullname] = mmap(kname).map(vname => {
          if (nochange contains vname.names(0)) { vname } 
          else { vname.prefixed(prefix) }
        })
        (nkey -> nset) 
      }).toMap
  }

  implicit class Order(o: ClassOrder) {
    def addPrefix(prefix: String, nochange: Set[String]): ClassOrder =
      o.copy(edges = o.edges.addPrefix(prefix, nochange))
    def merge(o2: ClassOrder): ClassOrder = {
      ClassOrder(o.edges.merge(o2.edges)) // drop alias info
    }
  }

  /////////////////////////////////////////////////

  def fileToModule(file: File): Module = 
    try {
      if (file.getName().endsWith(".py")) { parseFile(file) }
      else { Module(List()) }
    } catch {
      case EmptyFileException => Module(List())
    }
  def moduleToOrder(ast: Module): ClassOrder = transferModule(GIVEN_CLASS_ORDER)(ast)
  
  def infoModuleToOrder(i: Info[Module]): ClassOrder = {
    val infoOrder = i.map(moduleToOrder(_))
    infoModularize(infoOrder) 
  }
  

  def getDirClassInfo(file: File): Info[ClassOrder] =
    if (file.isDirectory()) {
      walkFile(file)(fileToModule(_)).map(moduleToOrder(_))
    } else {
      walkFile(file.getParentFile())(fileToModule(_)).map(moduleToOrder(_))
    }

  // global module top names to not change
  val noChangeSet: Set[String] = Set("tensorflow", "numpy")

  def infoModularize(info: Info[ClassOrder]): ClassOrder = info match {
    case DirInfo(dname, ds, fs) => {
      val newDirOrders: List[ClassOrder] = ds.map((di: DirInfo[ClassOrder]) => 
        infoModularize(di).addPrefix(di.name, noChangeSet))
      val newFileOrders: List[ClassOrder] = 
        fs.map(x => x.info.addPrefix(x.filename, noChangeSet))
      (newDirOrders ++ newFileOrders).foldLeft(ClassOrder())(_.merge(_)) //.addPrefix(dname, noChangeSet)
    }
    case FileInfo(fname, o) => o.addPrefix(fname, noChangeSet)
  }

  def dirClassOrder(file: File): ClassOrder = {
    val info = getDirClassInfo(file)
    infoModularize(info)
  }
}

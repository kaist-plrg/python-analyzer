package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import sys.process._

object TransformDiff {  
  // order for top-level stmts
  // ImportStmt / ImportFromStmt < FunDef < _
  def stmtOrder(x: Stmt, y: Stmt): Boolean = x match {
    case x: ImportStmt => true
    case x: ImportFromStmt => true
    case x: FunDef => y match {
      case y: ImportStmt => false
      case y: ImportFromStmt => false
      case _ => true
    }
    case _ => false
  }

  def sortStmts(mod: Module): Module = mod match {
    case Module(body, tyIg) => {
      val sortedBody = body.sortWith(stmtOrder) 
    }
    ???
  }
}

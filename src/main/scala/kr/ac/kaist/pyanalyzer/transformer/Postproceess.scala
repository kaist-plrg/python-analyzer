package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

case object InvalidOnelineStmt extends Exception
trait PostProcess {
  def apply(ast: Node): Node = ???

  // TODO check if OnelineStmt contains StarExpr
  def checkOnelineStmt(stmt: Stmt): Unit = stmt match {
    case OnelineStmt(stmts) => ???
  }
  
}

package kr.ac.kaist.pyanalyzer.parser.ast

import kr.ac.kaist.pyanalyzer.util.Walker._

case class Module(body: List[Stmt] = Nil, tyIgnore: List[Int] = Nil, name: String = "") extends Node {
  def exists(pf: Stmt => Boolean): Boolean = body.exists {
    walkStmt[Boolean](pf, _ | _, false)(_)
  }
}

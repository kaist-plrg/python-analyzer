package kr.ac.kaist.pyanalyzer.parser.ast

case class Module(body: List[Stmt], tyIgnore: List[Int] = Nil) extends Node

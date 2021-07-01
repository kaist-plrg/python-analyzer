package kr.ac.kaist.pyanalyzer.parser.ast

///////////////////////////////////
// Comprehension
///////////////////////////////////
case class Comprehension(
  target: Expr,
  in: Expr,
  conds: List[Expr],
  async: Boolean = false
) extends Node

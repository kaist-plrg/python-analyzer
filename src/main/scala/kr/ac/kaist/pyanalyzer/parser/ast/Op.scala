package kr.ac.kaist.pyanalyzer.parser.ast

sealed trait Op extends Node

case class AugAssignOp(op: String) extends Op

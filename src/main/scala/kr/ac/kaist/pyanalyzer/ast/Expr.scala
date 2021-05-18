package kr.ac.kaist.pyanalyzer.ast

sealed trait Expr extends Node

case object StarExpr extends Expr

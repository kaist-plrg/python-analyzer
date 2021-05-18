package kr.ac.kaist.pyanalyzer.ast

sealed trait Token extends Node

case object EndMaker extends Token
case object NewLine extends Token
case class Name(s: String) extends Token
case class Number(n: Double) extends Token

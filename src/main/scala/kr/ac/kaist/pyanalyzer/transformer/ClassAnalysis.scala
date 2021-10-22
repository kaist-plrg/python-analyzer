package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.DirWalker._
import kr.ac.kaist.pyanalyzer.util.{ Info }
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import java.io.File
import scala.io.Source._

object ClassAnalysis {
  def fileToModule(file: File): Module = parseFile(file)
  def moduleToOrder(ast: Module): ClassOrder = transferModule(ClassOrder())(ast)

  def getDirClassInfo(file: File): Info[ClassOrder] =
    walkFile(file)(fileToModule(_)).map(moduleToOrder(_))
}

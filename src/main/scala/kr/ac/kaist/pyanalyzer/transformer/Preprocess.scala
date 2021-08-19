package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util._
import kr.ac.kaist.pyanalyzer.util.Walker._

// excpetions
case object ImportAtMiddle extends Exception

object Preprocess extends Preprocess
trait Preprocess {
  def apply(ast: Node): Node = ??? 
  
  // check if module m contains training loop
  def containsTL(m: Module): Boolean = m.exists(_ == Comment("# training loop"))
  def identifyTL(m: Module): TL = {
    val existsOptim = m.exists(_ match {
      case ExprStmt(Call(Attribute(_, Id("fit")), _, _)) => true
      case _ => false
    })
    val existsTape = m.exists(_ match {
      case ExprStmt(Call(Attribute(_, Id("apply_gradients")), _, _)) => true
      case _ => false
    })
    if (!existsOptim && existsTape) GradientTape
    else if (existsOptim && !existsTape) Optimizer
    else if (!existsOptim && !existsTape) NoTL
    else throw new RuntimeException()
  }

  // checks if given Module node conforms to the transformation restriction
  // throws exception if violates restriction
  def checkRestriction(mod: Module): Unit = {
    // 1. all imports must be placed at the top of module
    checkImportOnlyTop(mod) 
  }
       
  // 1. all imports must be placed at the top of module
  def removeTopImports(stmts: List[Stmt]): List[Stmt] = stmts match {
    case ImportStmt(_) :: tail => removeTopImports(tail)
    case ImportFromStmt(_, _, _) :: tail => removeTopImports(tail)
    case _ => stmts
  }
  def throwIfImport(s: Stmt): Unit = s match {
    case ImportStmt(_) => throw ImportAtMiddle
    case ImportFromStmt(_, _, _) => throw ImportAtMiddle
    case _ => 
  }
  def checkImportOnlyTop(mod: Module): Unit = {
    val topImportsRemoved = removeTopImports(mod.body)
    topImportsRemoved.foreach(s => Walker.walkStmtUnit(throwIfImport)(s))  
  } 
}

// training loop
trait TL

case object Optimizer extends TL

case object GradientTape extends TL

case object NoTL extends TL

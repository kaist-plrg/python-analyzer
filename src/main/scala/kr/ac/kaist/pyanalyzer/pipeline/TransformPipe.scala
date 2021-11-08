package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info, FileInfo, DirInfo }

case class NotDir(name: String) extends Exception(name)
case class NotFile(name: String) extends Exception(name)
case class TargetNotFound(name: String) extends Exception(name)

case object TransformPipe extends Pipeline[
  (Info[Module], Info[ModuleSummary], ClassOrder, Option[String]), 
  Info[Module]
] {
  // some helpers
  def removeDirSummary(i: DirInfo[(Module, ModuleSummary)]): DirInfo[Module] =
    i.map(_._1) match {
      case e: FileInfo[Module] => throw NotDir(i.name) 
      case e: DirInfo[Module] => e
    }
  def removeFileSummary(i: FileInfo[(Module, ModuleSummary)]): FileInfo[Module] =
    i.map(_._1) match {
      case e: DirInfo[Module] => throw NotFile(i.name) 
      case e: FileInfo[Module] => e
    }

  def findTargetLoopType(summaries: Info[ModuleSummary], targetName: String): TLType  = {
    // must be DirInfo
    summaries match {
      case DirInfo(name, dirs, fs) => {
        // ASSUME: target file exist in fs
        val targetFiles = fs.filter(fi => fi.name == targetName)
        if (targetFiles.isEmpty) { throw TargetNotFound(targetName) }
        // extract the file loop type
        val targetLoopType = targetFiles.head.info.tl
        // return the loop type
        targetLoopType
      }
      case FileInfo(_, _) => throw NotDir(summaries.name) 
    }
  }

  def execute(
    p: (Info[Module], Info[ModuleSummary], ClassOrder, Option[String])
  ): Info[Module] = {
    // destructuring
    val (orgASTs, loopTypes, classOrder, targetOpt) = p
    // case by case
    targetOpt match {
      // target file name specified case
      case Some(targetName) => {
        // ast must be DirInfo
        orgASTs match {
          case DirInfo(name, dirs, fs) => {
            // ASSUME: target file exist in fs
            // find a file AST that has targetName, transform it
            val targetTL = findTargetLoopType(loopTypes, targetName) 
            val newFiles = fs.map(fi => {
              if (fi.name == targetName) {
                // transform the target file 
                fi.copy(info=Transformer(fi.info, classOrder, targetTL)) 
              } 
              else { fi }
            })
            // return same DirInfo with newFiles
            DirInfo(name, dirs, newFiles)
          }
          case FileInfo(_, _) => throw NotDir(orgASTs.name)
        }  
      }
      // target file not specified case: should check loopTypes 
      case None => {
        ???
      }
    }
  } 
}

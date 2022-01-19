package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info, FileInfo, DirInfo }

case class NotDir(name: String) extends Exception(name)
case class NotFile(name: String) extends Exception(name)
case class TargetNotFound(name: String) extends Exception(name)
case class DuplicateTargetFound(name: String) extends Exception(name)

case object MainScriptPipe extends Pipeline[
  (Info[Module], Info[ModuleSummary], Option[String]), (Module, APIType)
] {

  def findTargetLoopType(summaries: Info[ModuleSummary], targetName: String): APIType  = {
    // must be DirInfo
    summaries match {
      case DirInfo(name, dirs, fs) => {
        // ASSUME: target file exist in fs
        val targetFiles = fs.filter(fi => fi.name == targetName)
        if (targetFiles.isEmpty) { throw TargetNotFound(targetName) }
        // extract the file loop type
        val targetLoopType = targetFiles.head.info.api
        // return the loop type
        targetLoopType
      }
      case FileInfo(_, _) => throw NotDir(summaries.name) 
    }
  }

  def findTargetWithoutName (summaries: Info[ModuleSummary]): (String, APIType) = {
    // must be dirinfo
    summaries match {
      case FileInfo(_, _) => throw NotDir(summaries.name)
      case DirInfo(name, dirs, fs) => {
        val targetFiles = fs.filter(fi => fi.info.api != Bot)
        if (targetFiles.isEmpty) { throw TargetNotFound("") }
        if (targetFiles.length > 1) { throw DuplicateTargetFound("") }
        // only one target file exists
        val targetFile = targetFiles(0)
        (targetFile.name, targetFile.info.api) 
      }
    }
  }

  def execute(
    p: (Info[Module], Info[ModuleSummary], Option[String])
  ): (Module, APIType) = {
    val (orgASTs, loopTypes, targetOpt) = p
    orgASTs match {
      case DirInfo(name, dirs, fs) => {
        val (targetName, targetAPI) = targetOpt match {
          case Some(s) => (s, findTargetLoopType(loopTypes, s))
          case None => findTargetWithoutName(loopTypes)
        }
        // ASSUME: mainscript file exist in fs
        // find a file AST that has targetName
        fs.find(fi => fi.name == targetName) match {
          case Some(info) => (info.info, targetAPI)
          case None => throw TargetNotFound(targetName)
        }
      }
      case FileInfo(_, _) => throw NotDir(orgASTs.name)
    }
  }

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
}

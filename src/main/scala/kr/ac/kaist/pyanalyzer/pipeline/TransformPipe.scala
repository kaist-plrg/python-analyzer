package kr.ac.kaist.pyanalyzer.pipeline

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.Env
import kr.ac.kaist.pyanalyzer.util.{ Info, FileInfo, DirInfo }

case class NotDir(name: String) extends Exception(name)
case class NotFile(name: String) extends Exception(name)

case object TransformPipe extends Pipeline[
  ((Info[(Module, ModuleSummary)], ClassOrder), Option[String]), 
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


  def execute(
    p: ((Info[(Module, ModuleSummary)], ClassOrder), Option[String])
  ): Info[Module] = {
    val ((info, order), opt) = p   
    info match {
      // should be DirInfo
      case FileInfo(_, _) => throw NotDir(info.name)
      case DirInfo(name, ds, fs) => opt match {
        // target filename given
        // assume: target file exist in fs (direct child)
        case Some(fname) => 
          DirInfo(
            name, 
            ds.map(di => removeDirSummary(di)), 
            fs.map(fi => {
            if (fi.name == fname) {
              val (mod, summ) = fi.info
              val transformedModule = Transformer(mod, order, summ.tl)
              FileInfo[Module](fi.name, transformedModule)
            } else {
              removeFileSummary(fi)
            }
          }))
        // should find target file by TLType
        case None => ???
      }   
    }
  } 
}

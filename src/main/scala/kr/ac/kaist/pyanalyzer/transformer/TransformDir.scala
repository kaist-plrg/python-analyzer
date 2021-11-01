package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.{ Info, FileInfo, DirInfo }

case class NotDirInfo(name: String) extends Exception(name)

object TransformDir {
  // assumption: training code exist as direct child of the directory
  def filterChildModuleByName(info: Info[Module], name: String): Option[Module] =
    info match {
      case FileInfo(_, _) => throw NotDirInfo(info.name) 
      case DirInfo(_, _, fs) => ???
    }
}

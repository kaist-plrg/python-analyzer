package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.{ Info, DirInfo, FileInfo }
import scala.io.Source

case object DumpPipe extends Pipeline[(Info[File], String, String, Module), Unit] {
  def dumpInfo(
    info: Info[File],
    m: Module,
    base: String,
    path: String = ""
  ): Unit = info match {
    case DirInfo(name, subdirs, files) => 
      subdirs.map(dumpInfo(_, m, base, s"$path/$name/"))
      files.map(dumpInfo(_, m, base, s"$path/$name/"))
    case FileInfo(name, _) =>
      val file = new File(s"$base/$path", name)
      if (m.name == name) writeFile(file, beautify(m))
  }
  def execute(p: (Info[File], String, String, Module)): Unit = {
    val (fs, inPath, outPath, m) = p
    executeCmd(s"""bash -c "cp -r $inPath/* $outPath"""")
    fs match {
      case DirInfo(name, subdirs, files) =>
        subdirs.map(dumpInfo(_, m, outPath))
        files.map(dumpInfo(_, m, outPath))
      case finfo: FileInfo[File] => dumpInfo(finfo, m, outPath)
    }
  }
}

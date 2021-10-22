package kr.ac.kaist.pyanalyzer.util

import java.io.File

case class NotDirectory(s: String) extends Exception(s)
case class NotFile(s: String) extends Exception(s)

sealed trait Info[T] {
  def name: String = this match {
    case DirInfo(dname, _, _) => dname
    case FileInfo(fname, _) => fname
  }
}

// Wrapper for Directory
case class DirInfo[T](
  dirname: String, 
  subdirs: List[DirInfo[T]], 
  files: List[FileInfo[T]], 
) extends Info[T]

// Wrapper for File
case class FileInfo[T](
  filename: String,
  info: T // the actual info
) extends Info[T] 

object DirWalker {
  // case class filters
  def onlyDir[T](info: Info[T]): DirInfo[T] = info match {
    case (finfo: DirInfo[T]) => finfo
    case _ => throw NotDirectory(info.name)
  }

  def onlyFile[T](info: Info[T]): FileInfo[T] = info match {
    case (finfo: FileInfo[T]) => finfo
    case _ => throw NotFile(info.name)
  }

  // generates Info[T] with given f: file => T
  def walkFile[T](file: File)(f: File => T): Info[T] = 
    file.listFiles().toList.partition(_.isDirectory()) match {
      case (dirList, fileList) =>
        // make info for subdirectories
        val dirInfos: List[DirInfo[T]] = 
          dirList.map(walkFile[T](_)(f)).map(onlyDir[T])
        // make info for files directly inside
        val fileInfos: List[FileInfo[T]] =
          fileList.map(file => FileInfo(file.getName(), f(file)))

        DirInfo[T](file.getName(), dirInfos, fileInfos)
    }
  
  // the "map" function
  def map[A, B](info: Info[A])(convert: A => B): Info[B] = info match {
    case DirInfo(dname, ds, fs) =>
      DirInfo[B](
        dname, 
        ds.map(adi => map[A, B](adi)(convert)).map(onlyDir[B]),
        fs.map(afi => map[A, B](afi)(convert)).map(onlyFile[B])
      )
    case FileInfo(fname, i) => FileInfo[B](fname, convert(i))
  }
}

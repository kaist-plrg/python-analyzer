package kr.ac.kaist.pyanalyzer.util

import java.io.File
import kr.ac.kaist.pyanalyzer.util.Errors.EmptyFileException
import scala.util.Try

case class NotDirectory(s: String) extends Exception(s)
case class NotFile(s: String) extends Exception(s)

sealed trait Info[T] {
  def name: String = this match {
    case DirInfo(dname, _, _) => dname
    case FileInfo(fname, _) => fname
  }
  // the "map" function
  def map[U](convert: T => U): Info[U] = this match {
    case DirInfo(dname, ds, fs) =>
      DirInfo[U](
        dname, 
        ds.map(adi => adi.map[U](convert)).map(DirWalker.onlyDir[U](_)),
        fs.map(afi => afi.map[U](convert)).map(DirWalker.onlyFile[U](_))
      )
    case FileInfo(fname, i) => FileInfo[U](fname, convert(i))
  }
  def flatMap[U](f: T => Option[U]): Info[U] = this match {
    case DirInfo(dname, ds, fs) =>
      DirInfo[U](
        dname, 
        ds.map(_.flatMap[U](f)),
        fs.flatMap(finfo => Try(finfo.copy(info=f(finfo.info).get)).toOption)
      )
    // flatMap should not be directly called on FileInfo
    case FileInfo(fname, i) => ???
  }

  implicit def info2dinfo[T](info: Info[T]): DirInfo[T] = info match {
    case dinfo: DirInfo[T] => dinfo
    case _ => ???
  }
  implicit def info2finfo[T](info: Info[T]): FileInfo[T] = info match {
    case finfo: FileInfo[T] => finfo
    case _ => ???
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
  def walkFile[T](file: File)(f: File => T): Info[T] = {
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
  }
  
  def flatWalkFile[T](file: File)(f: File => Option[T]): DirInfo[T] = {
    file.listFiles().toList.partition(_.isDirectory()) match {
      case (dirList, fileList) =>
        // make info for subdirectories
        val dirInfos: List[DirInfo[T]] =
          dirList.map(flatWalkFile[T](_)(f))
        // make info for files directly inside
        val fileInfos: List[FileInfo[T]] = fileList.flatMap (file =>
          Try(FileInfo(file.getName(), f(file).get)).toOption
        )

        DirInfo[T](file.getName(), dirInfos, fileInfos)
    }
  }

  implicit def info2dinfo[T](info: Info[T]): DirInfo[T] = info match {
    case dinfo: DirInfo[T] => dinfo
    case _ => ???
  }
  implicit def info2finfo[T](info: Info[T]): FileInfo[T] = info match {
    case finfo: FileInfo[T] => finfo
    case _ => ???
  }
}

package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.hierarchy.ClassAnalysis._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import kr.ac.kaist.pyanalyzer.util.DirInfo
import scala.Console._
import scala.util.Try
import java.io.File

object Transform {
  def apply(optionMap: Map[String, String]): Unit = {
    // set target file
    val target = try {
      s".*${optionMap.getOrElse("target", "")}.*".r
    } catch {
      case e: Throwable => ".*".r
    }
    // diff files
    val diff = optionMap.getOrElse("diff", "hvd-hvd-trans")
    lazy val diffParser = "[^-]*".r ~ ("-" ~> ".*".r)
    val diffTarget = parse(diffParser, diff)
    // set diff option
    val diffOption = if (optionMap contains "y") "y" else "u"
    val dump = if (optionMap contains "d") true else false

    val hvd = new File(HOROVOD_DIR)

    for {
      versionDir <- hvd.listFiles if versionDir.isDirectory
      moduleDir <- versionDir.listFiles if moduleDir.isDirectory
      modelDir <- moduleDir.listFiles if modelDir.isDirectory
      modelPath = modelDir.toString
      val relPath = modelPath diff HOROVOD_DIR if target matches modelPath
    } try {
      println
      println(s"$MAGENTA$relPath$RESET")
      println

      val orgFile = PathPipe!!(s"$modelDir/org/")
      val orgInfo =
        (ParsePipe!!(orgFile)).asInstanceOf[DirInfo[Module]]
        .copy(dirname=s"$relPath/org")
      val classInfo = ClassPipe!!(orgInfo)
      val tlInfo = InfoTLPipe!!((orgInfo, classInfo))
      val transInfo =
        (TransformPipe!!((orgInfo, tlInfo, classInfo, None)))
        .asInstanceOf[DirInfo[Module]]
        .copy(dirname=s"$relPath/hvd-trans")
      val hvdFile = PathPipe!!(s"$modelDir/hvd/")
      val hvdInfo =
        (ParsePipe!!(hvdFile))
        .asInstanceOf[DirInfo[Module]]
        .copy(dirname=s"$relPath/hvd")

      // dump
      if (dump) {
        executeCmd(s"rm -rf $modelPath/hvd-trans", modelPath)
        dumpInfoModule(transInfo, new File(HOROVOD_DIR))
      }

      val diffDir = s"$TRANS_LOG_DIR/diff"
      // diff
      dumpInfoModule(orgInfo, new File(diffDir))
      dumpInfoModule(hvdInfo, new File(diffDir))
      dumpInfoModule(transInfo, new File(diffDir))

      val (path1, path2) = diffTarget match {
        case Success(a ~ b, _) =>
          (
            s"$TRANS_LOG_DIR/diff/$relPath/$a",
            s"$TRANS_LOG_DIR/diff/$relPath/$b"
          )
        case _ => ???
      }

      try executeCmd(s"colordiff -$diffOption $path1 $path2")
      catch {
        case e: Throwable =>
          executeCmd(
            s"diff -$diffOption $path1 $path2"
          )
          println
          println(s"${YELLOW}[Warning] install colordiff${RESET}")
      }
      executeCmd(s"rm -rf $TRANS_LOG_DIR/diff", modelPath)
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}

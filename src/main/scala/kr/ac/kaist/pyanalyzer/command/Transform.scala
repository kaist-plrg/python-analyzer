package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.pipeline.TransformRunner._
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
      if target matches modelPath
    } try {
      println
      println(s"$MAGENTA${modelPath diff HOROVOD_DIR}$RESET")
      println

      val transInfo =
        runPipe(s"$modelPath/org").asInstanceOf[DirInfo[Module]]
        .copy(dirname=s"${modelPath diff HOROVOD_DIR}/hvd-trans")

      // dump
      if (dump) {
        executeCmd(s"rm -rf /$modelPath/hvd-trans", modelPath)
        dumpInfoModule(transInfo, new File(HOROVOD_DIR))
      }

      // diff
      val (path1, path2) = diffTarget match {
        case Success(a ~ b, _) => (s"$modelPath/$a", s"$modelPath/$b")
        case _ => ???
      }
      println(s"colordiff -$diffOption $path1 $path2")
      try executeCmd(s"colordiff -$diffOption $path1 $path2")
      catch {
        case e: Throwable =>
          executeCmd(
            s"diff -$diffOption $path1 $path2"
          )
          println
          println(s"${YELLOW}[Warning] install colordiff${RESET}")
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}

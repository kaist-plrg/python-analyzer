package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.DirInfo
import scala.Console._
import scala.sys.process._
import scala.util.Try
import java.io.File

object Transform {
  def apply(optionMap: Map[String, String]): Unit = {
    val path = optionMap.get("inPath") match {
      case Some(p) => p
      case None =>
        println(s"${YELLOW}[Warning] no path is given$RESET")
        return
    }
    val outPath = optionMap.get("outPath") match {
      case Some(p) => p
      case None => TRANS_LOG_DIR
    }

    val diff = optionMap contains "diff"

    val hvd = new File(HOROVOD_DIR)

    try {
      // println
      // println(s"$MAGENTA$path$RESET")
      // println

      TransformRunner.runPipe(path, outPath, diff)
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}

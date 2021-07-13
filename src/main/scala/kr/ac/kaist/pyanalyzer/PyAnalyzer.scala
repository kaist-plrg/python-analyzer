package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.Command._
import kr.ac.kaist.pyanalyzer.util.Useful._

object PyAnalyzer {
  // init directory, etc
  def init(): Unit = {
    mkdir(TEST_LOG_DIR)
  }

  // main entry point
  def main(args: Array[String]): Unit = {
    init
    args.toList match {
      case str :: params => cmdMap.get(str) match {
        case Some(cmd) => cmd(params)
        case None => CmdHelp(List(str))
      }
      case Nil => CmdHelp(List())
    }
  }
}

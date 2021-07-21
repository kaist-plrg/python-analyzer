package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Transform {
  def apply(params: List[String]): Unit = {
    var DEBUG = params contains "-d"
    val target = params.headOption match {
      case Some(option) if option startsWith "-target:" =>
        Some(option.drop(8))
      case opt => None
    }
    val files = walkTree(HOROVOD_DIR)
    try for {
      file <- files
      orgPath = file.getPath()
      if (orgPath endsWith ".py") && (orgPath contains "/org/")
      relPath = orgPath.drop(HOROVOD_DIR.length + 1)
      if target.map(relPath contains _).getOrElse(true)
    } {
      if (DEBUG) scala.io.StdIn.readLine match {
        case "q" => throw new RuntimeException("quit")
        case "c" => DEBUG = false
        case _ => true
      }
      // parse
      println
      println(s"$CYAN$relPath$RESET")
      try {
        val orgText = readSource(orgPath)
        if (orgText == "") println(s"${RED}empty file${RESET}")
        else {
          val orgTokens = tokenizeText(orgText)
          val orgAst = TokenListParser(orgTokens)
          val orgResult = beautify(orgAst)
          // transform
          val transformedAst = Transformer(orgAst)
          val transformedResult = beautify(transformedAst)
          val hvdPath = HOROVOD_DIR + "/" + 
            relPath.replace("/org/", "/hvd/")
          val hvdText = readSource(hvdPath)
          val hvdTokens = tokenizeText(hvdText)
          val hvdAst = TokenListParser(hvdTokens)
          val hvdResult = beautify(hvdAst)
          dumpFile(transformedResult, BASE_DIR+"/org")
          dumpFile(hvdResult, BASE_DIR+"/hvd")
          println
          println(s"${CYAN}ORG${RESET}")
          println(orgResult)
          println("==================================================")
          println(s"${CYAN}TRANS${RESET}")
          println(transformedResult)
          println("==================================================")
          println(s"${CYAN}HVD${RESET}")
          println(hvdResult)
          println("==================================================")
          println(s"${CYAN}DIFF${RESET}")
          executeCmd(s"diff org hvd")
          executeCmd(s"rm org hvd")
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}

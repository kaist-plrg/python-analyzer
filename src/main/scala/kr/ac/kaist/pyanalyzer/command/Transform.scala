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
  def apply(optionMap: Map[String, String]): Unit = {
    val target = optionMap.get("target")
    val files = walkTree(HOROVOD_DIR)
    for {
      file <- files
      orgPath = file.getPath()
      if (orgPath endsWith ".py") && (orgPath contains "/org/")
      relPath = orgPath.drop(HOROVOD_DIR.length + 1)
      if target.map(relPath contains _).getOrElse(true)
    } {
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
          val transLogPath = BASE_DIR + "/logs/transform"
          mkdir(transLogPath)
          dumpFile(transformedResult, transLogPath + "/trans")
          dumpFile(hvdResult, transLogPath + "/hvd")
          println(s"${CYAN}DIFF${RESET}")
          try executeCmd(
            s"colordiff -u ${transLogPath}/hvd ${transLogPath}/trans")
          catch {
            case e: Throwable =>
              executeCmd(s"diff -u ${transLogPath}/hvd ${transLogPath}/trans")
              println
              println(s"${YELLOW}[Warning] install colordiff${RESET}")
          }
          executeCmd(s"rm ${transLogPath}/hvd ${transLogPath}/trans")
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}

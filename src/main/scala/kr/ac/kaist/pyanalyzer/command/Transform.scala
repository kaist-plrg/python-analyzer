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
      path = file.getPath() if path endsWith ".py"
      relPath = path.drop(HOROVOD_DIR.length + 1)
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
        val text = readSource(path)
        if (text == "") println(s"${RED}empty file${RESET}")
        else {
          val tokens = tokenizeText(text)
          val ast = TokenListParser(tokens)
          println(beautify(ast))
          println("==================================================")
          // transform
          println(beautify(Transformer(ast)))
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}

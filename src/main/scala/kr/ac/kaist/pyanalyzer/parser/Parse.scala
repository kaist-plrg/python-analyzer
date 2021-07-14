package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Parse {
  def apply(params: List[String]): Unit = {
    var DEBUG = params contains "-d"
    val target = params.headOption match {
      case Some(option) if option startsWith "-target:" => Some(option.drop(8))
      case opt => None
    }

    // TODO: handle the AST
    // TODO: refactor verbous call
    val files = walkTree(HOROVOD_DIR)
    try for {
      file <- files
      path = file.getPath() if path endsWith ".py"
      relPath = path.drop(HOROVOD_DIR.length + 1)
      if target.map(relPath contains _).getOrElse(true)
    } {
      // this path is for simple debugging
      // val path = s"$BASE_DIR/test.py"
      if (DEBUG) scala.io.StdIn.readLine match {
        case "q" => throw new RuntimeException("quit")
        case "c" => DEBUG = false
        case _ => true
      }
      println
      println(s"$CYAN$relPath$RESET")
      try {
        val text = readSource(path)
        val tokens = tokenizeText(text)
        val ast = TokenListParser(tokens).get
        println(beautify(ast))
      } catch {
        case e: Throwable => println(e.getMessage)
      }
      println
    } catch {
      case e: RuntimeException if e.getMessage == "quit" =>
    }
  }
}

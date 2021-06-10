package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer.LINE_SEP
import kr.ac.kaist.pyanalyzer.PyAnalyzer.Command
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Token._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import org.jline.builtins.Completers.TreeCompleter
import org.jline.builtins.Completers.TreeCompleter._
import org.jline.reader._
import org.jline.terminal._
import scala.Console._
import scala.util.parsing.input.Reader

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = "hihi"

  // TODO: add completer
  private val completer: TreeCompleter = new TreeCompleter(node(""))
  private val terminal: Terminal = TerminalBuilder.builder().build()
  private val reader: LineReader = LineReaderBuilder.builder()
    .terminal(terminal).completer(completer).build()
  private val prompt: String = LINE_SEP + s"${MAGENTA}py-analyze>${RESET} "
  def apply(params: List[String]): Unit = {
    try while (true) {
      val str = reader.readLine(prompt)
      val tokens = parseText(str)
      // TODO change the quit command
      tokens.headOption match {
        case Some(Delim(",")) => throw new EndOfFileException
        case _ =>
      }
      println(tokens)
      val e = expression(new PackratReader(TokenReader(tokens)))
      println(e)
    } catch {
      case e: EndOfFileException => println("quit")
    }
  }
}

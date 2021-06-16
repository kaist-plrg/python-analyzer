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

  val commands = List(
    "quit",
  ).map(x => node(s":$x"))

  val prodNode = prodMap.keys.map(
    x => node(s"-$x".toLowerCase)
  ).toList
  // TODO: add completer
  private val completer: TreeCompleter = new TreeCompleter(
    node(":raw" :: prodNode: _*) :: commands: _*
  )
  private val terminal: Terminal = TerminalBuilder.builder().build()
  private val reader: LineReader = LineReaderBuilder.builder()
    .terminal(terminal).completer(completer).build()
  private val prompt: String =
    LINE_SEP + s"${MAGENTA}py-analyze>${RESET} "
  def apply(params: List[String]): Unit = {
    try while (true) {
      val str = reader.readLine(prompt)
      val pairOpt = str.split(" ").toList match {
        case Nil => None
        case cmd :: rest if cmd.startsWith(":") => cmd.drop(1) match {
          case "quit" => throw new EndOfFileException
          case "raw" => rest match {
            case Nil => Some(None, "")
            case prod :: target if prod.startsWith("-") =>
              Some(Some(prod.drop(1)), target.mkString(" "))
            case target => Some(None, target.mkString(" "))
          }
          // TODO add production
          case _ => println("In appropriate command!"); None
        }
        case target => Some(None, target.mkString(" "))
      }
      pairOpt.map(pair => {
        val tokens = parseText(pair._2)
        println(tokens)
        val prodName = pair._1.getOrElse("expression")
        println(s"Parsing with production, $prodName")
        val e = prodMap.getOrElse(prodName.capitalize, expression)(new PackratReader(TokenReader(tokens)))
        println(e)
      })
    } catch {
      case e: EndOfFileException => println("quit")
    }
  }
}

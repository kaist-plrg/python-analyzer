package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer.LINE_SEP
import kr.ac.kaist.pyanalyzer.PyAnalyzer.Command
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Token._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.Useful._
import org.jline.builtins.Completers.TreeCompleter
import org.jline.builtins.Completers.TreeCompleter._
import org.jline.reader._
import org.jline.terminal._
import scala.Console._
import scala.util.parsing.input.Reader
import scala.util.Try

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = "parse the expression on the REPL"

  val commands = List(
    "quit",
  ).map(x => node(s":$x"))

  val prodNode = prodMap.keys.map(
    x => node(s"-${x.charAt(0).toLower}${x.drop(1)}")
  ).toList

  private val completer: TreeCompleter = new TreeCompleter(
    node(":raw" :: prodNode: _*) :: commands: _*
  )

  private val terminal: Terminal = TerminalBuilder.builder().build()

  private val reader: LineReader = LineReaderBuilder.builder()
    .terminal(terminal).completer(completer).build()

  private val prompt: String =
    LINE_SEP + s"${MAGENTA}py-analyze>${RESET} "

  // main apply : loop to get input line, and try parsing the line
  def apply(params: List[String]): Unit = {
    try while (true) {
      // 1. get input string, identify if it's empty, command starting with `:`,
      // or target string to pars
      val str = reader.readLine(prompt)
      val pairOpt = str.split(" ").toList match {
        // empty string
        case Nil => None
        // command case, starting with `:`
        case cmd :: rest if cmd.startsWith(":") => cmd.drop(1) match {
          case "quit" => throw new EndOfFileException
          case "raw" => rest match {
            case Nil => Some(None, "")
            case prod :: target if prod.startsWith("-") =>
              Some(Some(prod.drop(1)), target.mkString(" "))
            case target => Some(None, target.mkString(" "))
          }
          case _ => println("In appropriate command!\nAvailable commands are [:quit, :raw]"); None
        }
        // target string case
        case _ => Some(None, str)
      }
      //2. according to result, do the actual parsing
      pairOpt.map(pair => {
        val targetLine = pair._2
        val prodName = pair._1.getOrElse("statement") 
        println(s"${CYAN}Target String:${RESET} ${targetLine}")
        
        val tokens = tokenizeText(targetLine)
        println(s"${GREEN}Tokenize result:${RESET} ${tokens}")

        println(s"${GREEN}Goal production:${RESET} ${prodName}")

        val parseResult = prodMap.getOrElse(prodName.capitalize, statement)(
          new PackratReader(TokenListParser.TokenReader(tokens))
        )
        println(s"${GREEN}Parse result${RESET}: ${parseResult}")

        try {
          val prettyResult = beautify(parseResult.get)
          println(s"${CYAN}Beautify result${RESET}: ${prettyResult}")
        } catch {
          case e: Throwable => println(e)
        }
      })
    // -1. End when EOF thrown (:quit) case
    } catch {
      case e: EndOfFileException => println("quit")
    }
  }
}

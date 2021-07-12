package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer.LINE_SEP
import kr.ac.kaist.pyanalyzer.PyAnalyzer.Command
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Token._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.Useful._
import org.jline.builtins.Completers.TreeCompleter
import org.jline.builtins.Completers.TreeCompleter.{Node => CNode, node}
import org.jline.reader._
import org.jline.terminal._
import scala.Console._
import scala.util.parsing.input.Reader
import scala.util.Try

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = s"""${CYAN}
  Parse the stmt/expr/subterms on the REPL
  You can give command starts with ":"
  Using tab is helpful for checking the command and option
${RESET}"""

  val commandList = List(
    "quit",
  )
  val commands = commandList.map(x => node(s":$x"))
  val commandHelp =
    commandList.foldLeft("Commands: raw")((str, e) => str + s", $e")

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

  // main apply: get input line and try parsing the line
  def apply(params: List[String]): Unit = {
    println(help)
    try while (true) try {
      val str = reader.readLine(prompt)
      val (prodOpt, targetStr) = str.split(" ").toList match {
        case cmd :: rest if cmd.startsWith(":") => cmd.drop(1) match {
          case "quit" => throw new EndOfFileException
          case "raw" => rest match {
            case Nil => (None, "")
            case prod :: target if prod.startsWith("-") =>
              (Some(prod.drop(1)), target.mkString(" "))
            case target => (None, target.mkString(" "))
          }
          case _ => throw new RuntimeException(
            "In appropriate command!\n" + commandHelp
          )
        }
        case _ => (None, str)
      }
      val prodName = prodOpt.getOrElse("statements")
      println(s"${GREEN}Goal production:${RESET} ${prodName}\n")

      val tokens = tokenizeText(targetStr)
      println(s"${GREEN}Tokenize result:\n${RESET} ${tokens}\n")

      val parseResult = prodMap.getOrElse(prodName.capitalize, statements)(
        new PackratReader(TokenListParser.TokenReader(tokens))
      )
      println(s"${GREEN}Parse result${RESET}: ${parseResult}\n")

      parseResult.get match {
        case l: List[Node] =>
          val stmts = l.foldLeft("")((s, e) => s + beautify(e))
          println(s"${CYAN}Beautify result${RESET}:\n${stmts}\n")
        case node: Node =>
          println(s"${CYAN}Beautify result${RESET}:\n${beautify(node)}\n")
      }
    } catch {
      case e: EndOfFileException => throw new EndOfFileException
      case e: Throwable => println(e.getMessage)
    } catch {
      case e: EndOfFileException => println("quit")
    }
  }
}

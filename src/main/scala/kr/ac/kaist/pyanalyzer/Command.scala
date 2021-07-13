package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser.ParseREPL
import scala.Console._

object Command {
  val commands: List[Command] = List(
    CmdHelp,
    CmdParseREPL,
  )
  val cmdMap = commands.foldLeft(Map[String, Command]()) {
    case (map, command) => map + (command.name -> command)
  }
}

trait Command {
  val name: String
  val help: String
  def apply(params: List[String]): Any
}

case object CmdHelp extends Command {
  val name = "help"
  val help = ""
  def apply(params: List[String]): Unit = {
    params match {
      case cmd :: _ =>
        println
        println(s"${RED}[Error] $cmd: Command Not Found${RESET}")
      case Nil =>
        println
        println("<Command List>")
        for (command <- Command.commands if command != CmdHelp) 
          println(s"* ${command.name}: ${command.help}")
        println
    }
  }
}

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = s"""${CYAN}
  Parse the stmt/expr/subterms on the REPL
  You can give command starts with ":"
  Using tab is helpful for checking the command and option
${RESET}"""
  def apply(params: List[String]): Unit = ParseREPL(params)
}

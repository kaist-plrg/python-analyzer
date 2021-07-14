package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser.Parse
import kr.ac.kaist.pyanalyzer.parser.ParseREPL
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Command {
  val commands: List[Command] = List(
    CmdHelp,
    CmdParse,
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
        println(s"${CYAN}<Command List>${CYAN}")
        println
        for (command <- Command.commands if command != CmdHelp) 
          println(s"* ${CYAN}${command.name}${RESET}: ${command.help}")
        println
    }
  }
}

case object CmdParse extends Command {
  val name = "parse"
  val help = "Parse python code and build AST"
  def apply(params: List[String]): Unit = Parse(params)
}

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = "Parse REPL"
  def apply(params: List[String]): Unit = ParseREPL(params)
}

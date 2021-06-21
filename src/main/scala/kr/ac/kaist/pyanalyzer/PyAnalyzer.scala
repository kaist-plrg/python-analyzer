package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.CmdParseREPL
import scala.Console._

object PyAnalyzer {
  // main entry point
  def main(args: Array[String]): Unit = args.toList match {
    case str :: params => cmdMap.get(str) match {
      case Some(cmd) => cmd(params)
      case None => CmdHelp(List(str))
    }
    case Nil => CmdHelp(List())
  }

  val commands: List[Command] = List(
    CmdHelp,
    CmdParseREPL,
  )
  val cmdMap = commands.foldLeft(Map[String, Command]()) {
    case (map, command) => map + (command.name -> command)
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
          for (command <- commands if command != CmdHelp) 
            println(s"* ${command.name}: ${command.help}")
          println
      }
    }
  }
}

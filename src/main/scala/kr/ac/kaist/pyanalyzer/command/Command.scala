package kr.ac.kaist.pyanalyzer.command

import scala.util.parsing.combinator._
import kr.ac.kaist.pyanalyzer.command.Parse
import kr.ac.kaist.pyanalyzer.command.ParseREPL
import kr.ac.kaist.pyanalyzer.command.Transform
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Command {
  val commands: List[Command] = List(
    CmdHelp,
    CmdParse,
    CmdParseREPL,
    CmdTransform,
  )
  val cmdMap = commands.foldLeft(Map[String, Command]()) {
    case (map, command) => map + (command.name -> command)
  }
}

sealed trait Command {
  val name: String
  val help: String
  def apply(args: List[String]): Any
  implicit def parseArgs(args: List[String]): Map[String, String] =
    args.map(parseArg).flatten.toMap
  private def parseArg(arg: String): Option[(String, String)] = ArgParser(arg)
}

case object CmdHelp extends Command {
  val name = "help"
  val help = ""
  def apply(args: List[String]): Unit = {
    args match {
      case cmd :: _ =>
        println
        println(s"${RED}[Error] $cmd: Command Not Found${RESET}")
      case Nil =>
        println
        println(s"${CYAN}<Command List>${RESET}")
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
  def apply(args: List[String]): Unit = Parse(args)
}

case object CmdParseREPL extends Command {
  val name = "parse-repl"
  val help = "Parse REPL"
  def apply(args: List[String]): Unit = ParseREPL(args)
}

case object CmdTransform extends Command {
  val name = "transform"
  val help = "transform signle gpu code to multi gpu code"
  def apply(args: List[String]): Unit = Transform(args)
}

object ArgParser extends RegexParsers {
  def apply(arg: String): Option[(String, String)] = {
    parse(argParser, arg) match {
      case Success(res, rest) => Some(res)
      case _ => None
    }
  }

  lazy val argParser = "-" ~> "[^:]*".r ~ opt(":" ~> ".*".r) ^^ {
    case a ~ b => (a, b.getOrElse(""))
  }
}

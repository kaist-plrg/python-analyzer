package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer.PyAnalyzer.Command
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import kr.ac.kaist.pyanalyzer.parser.Grammar._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._
import scala.util.Try

object CheckProd {
  def apply(
    prod: String,
    index: Option[Int] = None,
    times: Int = 1
  ): Boolean = {
    (for {
      (testGenerator, i) <- PEG_Grammar(prod).zipWithIndex
        if index.getOrElse(i) == i
      time <- 1 to times
    } yield {
      val testname = s"$prod $i"
      println(s"$MAGENTA<$testname>$RESET")
      println
      val parser = prodMap(prod)
      doParse(parser, testGenerator) match {
        case Success(res, rest) if rest.first == Newline =>
          println("parsed result:")
          println(beautify(res))
          println
          true
        case res =>
          println(s"${RED}parsing failed$RESET")
          println(res)
          println
          false
      }
    }).forall(x => x)
  }

  def doParse[T](p: Parser[T], test: String) = {
    println("test string:")
    println(test)
    println
    p(new PackratReader(TokenReader(parseText(test))))
  }
}

case object CmdCheckProd extends Command {
  val name = "check-prod"
  val help = "check the specific production"
  def apply(params: List[String]): Unit = params match {
    case Nil => 
      println(s"${RED}[Error] Prod argument is needed${RESET}")
    case prod :: l => prodMap.get(prod) match {
      case Some(p) => 
        val times = Try(l.tail.head.toInt).toOption.getOrElse(1)
        Try(l.head.toInt).toOption match {
          case Some(index) => CheckProd(prod, Some(index), times)
          case None => CheckProd(prod, None, times)
        }
      case None =>
        println(s"${RED}[Error] Unknown production: ${prod}${RESET}")
    }
  }
}

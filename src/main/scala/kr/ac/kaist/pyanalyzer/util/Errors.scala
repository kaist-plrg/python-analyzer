package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer._

object Errors {
  case object EmptyFileException extends Exception
  case object APIException extends Exception
  case object ArgException extends Exception
}

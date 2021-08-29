package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.Console._
import java.io.File

case class Model(modules: List[Module])

object Transform {
  val logPath = s"$BASE_DIR/logs/transform"
  mkdir(logPath)

  def apply(optionMap: Map[String, String]): Unit = {
    // set target file
    val target = try {
      s".*${optionMap.getOrElse("target", "")}.*".r
    } catch {
      case e: Throwable => ".*".r
    }
    // diff files
    val diff = optionMap.getOrElse("diff", "hvd-trans")
    // set diff option
    val diffOption = if (optionMap contains "y") "y" else "u"

    val hvd = new File(HOROVOD_DIR)
    for {
      version <- hvd.listFiles
      module <- version.listFiles.filter(_.isDirectory)
      model <- module.listFiles if model.isDirectory
      if target matches model.toString
    } {
      println
      println(s"$MAGENTA$model$RESET")
      val files = walkTree(model)
      for {
        file <- files
        path = file.toString diff model.toString
        if path endsWith ".py"
        if path startsWith "/org/"
        name = path.drop(5)
      } try {
        println
        val orgAst = parseFile(s"$model$path").copy(name = Some(name))
        val orgResult = beautify(orgAst)

        // transformed
        val transformedAst = Transformer(orgAst).copy(name = Some(name))
        val transformedResult = beautify(transformedAst)
        // hvd
        val hvdAst = parseFile(s"$model/hvd/$name").copy(name = Some(name))
        val hvdResult = beautify(hvdAst)

        // target diff
        val comparePair = diff match {
          case "org-hvd" =>
            ("org", orgResult, "hvd", hvdResult)
          case "org-trans" =>
            ("org", orgResult, "trans", hvdResult)
          case _ =>
            ("hvd", hvdResult, "trans", transformedResult)
        }

        // print result
        printDiff(name, comparePair, diffOption)
      } catch {
        case EmptyFileException =>
        case e: Throwable => e.printStackTrace()
      }
    }
  }

  def printDiff(
    name: String,
    comparePair: (String, String, String, String),
    diffOption: String
  ): Unit = {
    val (name1, content1, name2, content2) = comparePair
    val path1 = s"$logPath/$name1"
    val path2 = s"$logPath/$name2"
    dumpFile(content1, path1)
    dumpFile(content2, path2)
    println(s"$CYAN$name DIFF$RESET :")
    try executeCmd(
      s"colordiff -$diffOption $path1 $path2"
    ) catch {
      case e: Throwable =>
        executeCmd(
          s"diff -$diffOption $path1 $path2"
        )
        println
        println(s"${YELLOW}[Warning] install colordiff${RESET}")
    }
    executeCmd(s"rm $path1 $path2")
  }
}

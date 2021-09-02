package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.Console._
import scala.util.Try
import java.io.File

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

      // get modules in the model
      val files = walkTree(model)
      val moduleOptions = for {
        file <- files
        path = file.toString diff model.toString
        if path endsWith ".py"
        if path startsWith "/org/"
        name = path.drop(5)
      } yield Try(parseFile(file.toString, name)).toOption
      val modules = moduleOptions.flatten

      // transform each module
      for (orgAst <- modules) try {
        println
        println(s"$CYAN<${orgAst.name}>$RESET")

        val orgResult = beautify(orgAst)
        
        val summary = TrainingLoop(modules, orgAst)
        println(summary)

        // transformed
        val transformedAst = Transformer(orgAst, summary.tl)
        val transformedResult = beautify(transformedAst)
        // hvd
        val hvdAst = parseFile(s"$model/hvd/${orgAst.name}")
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
        printDiff(comparePair, diffOption)
      } catch {
        case EmptyFileException =>
        case e: Throwable => e.printStackTrace()
      }
    }
  }

  def printDiff(
    comparePair: (String, String, String, String),
    diffOption: String
  ): Unit = {
    val (name1, content1, name2, content2) = comparePair
    val path1 = s"$logPath/$name1"
    val path2 = s"$logPath/$name2"
    dumpFile(content1, path1)
    dumpFile(content2, path2)
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

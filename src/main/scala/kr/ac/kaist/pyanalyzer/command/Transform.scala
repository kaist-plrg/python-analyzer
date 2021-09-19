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
      versionDir <- hvd.listFiles if versionDir.isDirectory
      moduleDir <- versionDir.listFiles if moduleDir.isDirectory
      modelDir <- moduleDir.listFiles if modelDir.isDirectory
      modelPath = modelDir.toString
      model = modelPath diff moduleDir.toString
      if target matches model.toString
    } {
      println
      println(s"$MAGENTA${modelPath diff HOROVOD_DIR}$RESET")
      println

      // get modules in the model
      val files = walkTree(modelPath)
      val moduleOptions = for {
        file <- files
        path = file.toString diff modelPath
        if path endsWith ".py"
        if path startsWith "/org/"
      } yield Try(
        parseFile(file.toString, path.drop(5))
      ).toOption
      val modules = moduleOptions.flatten
      val modelSummary = TrainingLoop(model, modules)
      println(modelSummary)

      // transform each module
      for (orgAst <- modules) try {

        val name = orgAst.name

        println
        println(s"$CYAN<$name>$RESET")

        val orgResult = beautify(orgAst)
        val summary = modelSummary.sumMap(name)

        // transformed
        val transformedAst = Transformer(orgAst, summary.tl)
        val transformedResult = beautify(transformedAst)
        // hvd
        val hvdAst = parseFile(s"$modelPath/hvd/$name")
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
        // printDiff(comparePair, diffOption)
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

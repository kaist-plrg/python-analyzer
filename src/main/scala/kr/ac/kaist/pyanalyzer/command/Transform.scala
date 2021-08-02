package kr.ac.kaist.pyanalyzer.command

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.SourceParser._
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Transform {
  val logPath = BASE_DIR + "/logs/transform"
  mkdir(logPath)

  def apply(optionMap: Map[String, String]): Unit = {
    // set target file
    val target = optionMap.get("target")
    // set diff option
    val diffOption = if (optionMap contains "y") "y" else "u"
    // set diff stage
    val stage = optionMap.getOrElse("stage", "hvd-trans")
    // print all stage
    val all = if (optionMap contains "all") true else false

    val files = walkTree(HOROVOD_DIR)
    for {
      file <- files
      orgPath = file.getPath()
      if (orgPath endsWith ".py") && (orgPath contains "/org/") &&
        readSource(orgPath).nonEmpty
      relPath = orgPath.drop(HOROVOD_DIR.length + 1)
      if target.map(relPath contains _).getOrElse(true)
      hvdPath = s"$HOROVOD_DIR/${relPath.replace("/org/", "/hvd/")}"
    } {

      println
      println(s"$MAGENTA$relPath$RESET")
      try {
        // org
        val orgAst = parseFile(orgPath)
        val orgResult = beautify(orgAst)
        // transformed
        val transformedAst = Transformer(orgAst)
        val transformedResult = beautify(transformedAst)
        // hvd
        val hvdAst = parseFile(hvdPath)
        val hvdResult = beautify(hvdAst)

        // target stage
        val comparePair = stage match {
          case "org-hvd" =>
            ("org", orgResult, "hvd", hvdResult)
          case "org-trans" =>
            ("org", orgResult, "trans", hvdResult)
          case _ =>
            ("hvd", hvdResult, "trans", transformedResult)
        }

        // print result
        if (all) {
          printResult(orgResult, transformedResult, hvdResult)
          println("==================================================")
        }
        printDiff(comparePair, diffOption)
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }

  def printResult(
    orgResult: String,
    transformedResult: String,
    hvdResult: String,
  ): Unit = {
    println
    println(s"${MAGENTA}ORG${RESET}")
    println(orgResult)
    println("==================================================")
    println(s"${MAGENTA}TRANS${RESET}")
    println(transformedResult)
    println("==================================================")
    println(s"${MAGENTA}HVD${RESET}")
    println(hvdResult)
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
    println(s"${MAGENTA}DIFF${RESET}")
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

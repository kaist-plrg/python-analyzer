package kr.ac.kaist.pyanalyzer.pipeline

import java.io.File
import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.parser.ast.Module
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline.PipelineOps
import kr.ac.kaist.pyanalyzer.pipeline.Pipeline._
import kr.ac.kaist.pyanalyzer.pipeline._
import kr.ac.kaist.pyanalyzer.transformer.{ ModuleSummary }
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.{ Info }

object TransformRunner {
  /* val subPipe: 
  Pipeline[Info[Module], (Info[(Module, ModuleSummary)], ClassOrder)] = 
    (idPipe ++ ClassPipe) >> (InfoTLPipe || idPipe[(Info[Module], ClassOrder)].snd) */

  /* val transformPipe = // String -> Info[Module], transform applied
    (PathPipe ++ CheckFilePipe) // String -> (File, Option[String])
      .fstMap(ParsePipe) 
        // (File, Option[String]) 
        // -> (Info[Module], Option[String]) 
      .fstMap(subPipe) >> TransformPipe
        // (Info[Module], Option[String]) 
        // -> ((Info[(Module, ModuleSummary)], ClassOrder), Option[String])
        // -> Info[Module] */
 
  //def run(path: String): Info[Module] = transformPipe!!(path)

  def runPipe(
    inPath: String,
    outPath: String = TRANS_LOG_DIR,
    diff: Boolean = false
  ): Unit = {
    val targetOpt = CheckFilePipe!!(inPath)
    val fs: Info[File] = PathPipe!!(inPath) 
    val orgASTs: Info[Module] = ParsePipe!!(fs) 
    val classOrder: ClassOrder = ClassPipe!!(orgASTs)
    val loopTypes: Info[ModuleSummary] = InfoTLPipe!!((orgASTs, classOrder)) 
    val (mainScriptAST, api) = MainScriptPipe!!((orgASTs, loopTypes, targetOpt))
    val transformAST: Module = TransformPipe!!((mainScriptAST, classOrder, api))
    mkdir(outPath)
    DiffPipe!!((outPath, mainScriptAST, transformAST))
    if (diff) {
      mkdir(s"$outPath/org/")
      mkdir(s"$outPath/trans/")
      DumpPipe!!((fs, inPath, s"$outPath/org/", Module(Nil, Nil, "")))
      DumpPipe!!((fs, inPath, s"$outPath/trans/", transformAST))
    }
    else DumpPipe!!((fs, inPath, outPath, transformAST))
  }
}

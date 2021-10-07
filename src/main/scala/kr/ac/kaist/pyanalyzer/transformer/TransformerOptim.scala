package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Preprocess._
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.transformer.TransformerTape
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object TransformerOptim extends Transformer {
  def apply(module: Module): Module = module.copy(body=transform(module.body)(Env())._1)

  override def transform(stmt: Stmt)(implicit env: Env): (List[Stmt], Env) = stmt match {
    /////////////////////////////////////////////////////////////////
    //// strict form of assignment
    /////////////////////////////////////////////////////////////////
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) => expr1 match {
      case Attribute(Attribute(Attribute(
        EName(idk), Id("keras")), Id("models")), Id("Sequential"))
      if env.get("tensor_flow") contains idk => (
        AssignStmt(List(EName(idr)), transform(Call(expr1, exprs, kwds)), ty),
        env.add("model", idr)
      )
      case _ => super.transform(stmt)
    }

    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportStmt(alias) =>
      val newEnv = transform(alias)
      val diffEnv = newEnv \ env
      // get "tensor_flow" id 
      diffEnv.get("tensor_flow") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newStmts = List(ImportStmt(alias)) ++ 
            parseStmts(stmtData("import-some")(List(id.name))) 
          (newStmts, newEnv)
        // corresponding not found
        case _ => (ImportStmt(alias), newEnv)
      }

    /////////////////////////////////////////////////////////////////
    // strict form of expr stmts
    case ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      case Attribute(EName(idt), Id("fit"))
      if env.get("model") contains idt => 
        val callbacks = parseExpr("[hvd.callbacks.BroadcastGlobalVariablesCallback(0)]")
        val cbKwarg = NormalKwarg(Id("callbacks"), callbacks)
        findKwarg(kwds, "callbacks") match {
          case Some(kwarg) =>
            val newkwds = replaceElement(kwds, kwarg, cbKwarg)
            (ExprStmt(Call(expr1, exprs, newkwds)), env)
          case None =>
            (ExprStmt(Call(expr1, exprs, kwds :+ cbKwarg)), env)
        }
      case _ => super.transform(stmt)
    }
    case _ => super.transform(stmt)
  }

  val stmtData: Map[String, List[String] => String] = Map(
    // import stmt
    "import-some" -> (names => { 
        val name = names(0)
        s"""import horovod.tensorflow.keras as hvd
           |hvd.init()
           |gpus = ${name}.config.experimental.list_physical_devices('GPU')
           |for gpu in gpus:
           |  ${name}.config.experimental.set_memory_growth(gpu, True)
           |if gpus:
           |  ${name}.config.experimental.\\
           |    set_visible_devices(gpus[hvd.local_rank()], 'GPU')""".stripMargin
    }),
  )
}

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
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        case Attribute(Attribute(Attribute(
          EName(idk), Id("keras")), Id("models")), Id("Sequential"))
        if env.get("tensor_flow") contains idk => (
          AssignStmt(targets, transform(Call(expr1, exprs, kwds)), ty),
          env.add("model", idr)
        )
        case Attribute(Attribute(EName(idk), Id("optimizers")), Id("Adam"))
        if env.get("keras") contains idk => (
          parseStmts(stmtData("assign-optimizer-default-adam")(List(idr.name))),
          env
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
        val verboseArg = parseExpr("1 if hvd.rank() == 0 else 0")
        val callbacksArg = parseExpr("[hvd.callbacks.BroadcastGlobalVariablesCallback(0)]")
        (findKwarg(kwds, "verbose"), findKwarg(kwds, "callbacks")) match {
          case (Some(vbKwarg), Some(cbKwarg)) =>
            val newVbKwarg = vbKwarg.copy(expr = verboseArg)
            val newCbKwarg =
              cbKwarg.copy(expr = EName(Id("callbacks")))
            val interkwds = replaceElement(kwds, vbKwarg, newVbKwarg)
            val newkwds = replaceElement(interkwds, cbKwarg, newCbKwarg)
            val existing_callbacks = beautify(cbKwarg.expr)
            val init_callbacks =
              parseStmts(stmtData("init-callbacks")(List(existing_callbacks)))
            (init_callbacks :+ ExprStmt(Call(expr1, exprs, newkwds)), env)
            // TODO: consider the case verbose or callbacks is not given
          case (Some(vbKwarg), None) =>
            val newVbKwarg = vbKwarg.copy(expr = verboseArg)
            val newCbKwarg = NormalKwarg(Id("callbacks"), callbacksArg)
            val newkwds = replaceElement(kwds, vbKwarg, newVbKwarg)
            (ExprStmt(Call(expr1, exprs, newkwds :+ newCbKwarg)), env)
          case (None, Some(cbKwarg)) =>
            val newVbKwarg = NormalKwarg(Id("verbose"), verboseArg)
            val newCbKwarg = cbKwarg.copy(expr = EName(Id("callbacks")))
            val newkwds = replaceElement(kwds, cbKwarg, newCbKwarg)
            val existing_callbacks = beautify(cbKwarg.expr)
            val init_callbacks =
              parseStmts(stmtData("init-callbacks")(List(existing_callbacks)))
            (init_callbacks :+ ExprStmt(Call(expr1, exprs, newkwds :+ newVbKwarg)), env)
          case (None, None) =>
            val newVbKwarg = NormalKwarg(Id("verbose"), verboseArg)
            val newCbKwarg = NormalKwarg(Id("callbacks"), callbacksArg)
            (ExprStmt(Call(expr1, exprs, kwds :+ newVbKwarg :+ newCbKwarg)), env)
        }
      case Attribute(EName(idt), Id("compile")) if env.get("model") contains idt =>
        val optim = Id("optim")
        findKwarg(kwds, "optimizer") match {
          case Some(kwarg) if kwarg.expr == EConst(StringLiteral("adam")) =>
            val newkwds = replaceElement(kwds, kwarg, kwarg.copy(expr = EName(optim)))
            val newStmts =
              parseStmts(stmtData("assign-optimizer-default-adam")(List(optim.name))) ++
              ExprStmt(Call(expr1, exprs, newkwds))
            (newStmts, env)
          case None if exprs.headOption contains EConst(StringLiteral("adam")) =>
            val newStmts =
              parseStmts(stmtData("assign-optimizer-default-adam")(List(optim.name))) ++
              ExprStmt(Call(expr1, EName(optim) :: exprs.tail, kwds))
            (newStmts, env)
          case _ => super.transform(stmt)
        }
      case EName(Id("print")) => {
        // hvd.rank()
        val rank = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
        // hvd.rank() == 0 
        val condExpr = CompExpr(rank, List((CEq,EConst(IntLiteral(0)))))
        // if hvd.rank() == 0: ...
        val ifStmt = IfStmt(condExpr, List(stmt), Nil)
        (List(ifStmt), env)
      }
      case Attribute(EName(idt), Id("summary")) if env.get("model") contains idt =>
        (parseStmts(stmtData("model-summary")(Nil)), env)
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
    "assign-optimizer-default-adam" -> (names => {
        val optim = names(0)
        s"""$optim = tf.optimizers.Adam(learning_rate=0.001 * hvd.size())
          |$optim = hvd.DistributedOptimizer($optim)""".stripMargin
    }),
    "init-callbacks" -> (names => {
        val existing_callbacks = names(0)
        s"""callbacks = [hvd.callbacks.BroadcastGlobalVariablesCallback(root_rank=0)]
          |if hvd.rank() == 0:
          |  callbacks.append($existing_callbacks)""".stripMargin
    }),
    "model-summary" -> (names =>
        s"""if hvd.rank() == 0: model.summary()"""
    ),
  )
}

package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.Preprocess._
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.transformer.TransformerTape
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object TransformerOptim extends TransformerMainScript {
  def apply(module: Module)(implicit env: Env = Env()): (Module, List[Warning]) = {
    val (stmts, _, lw) = transform(module.body)
    (module.copy(body=stmts), lw)
  }

  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    /////////////////////////////////////////////////////////////////
    //// strict form of assignment
    /////////////////////////////////////////////////////////////////
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        // TODO
        case _ if env.isSubclass(expr1, OPTIMIZER) =>
          val optimWrapping = getStmts("wrapping-optim", idr)
          // assume learning_rate is first positional argument
          (exprs.headOption, findKwarg(kwds, "learning_rate")) match {
            // keyword learning rate scheduler
            case (None, Some(NormalKwarg(_, EName(ids))))
              if env.get("lr_scheduler") contains ids =>
                (stmt :: optimWrapping, env.add("optimizer", idr))
            // keyword constant learning rate
            case (None, Some(kwarg)) =>
              val newExpr = parseExpr(s"(${beautify(kwarg.expr)}) * hvd.size()")
              val newKwarg = kwarg.copy(expr = newExpr)
              val newkwds = replaceElement(kwds, kwarg, newKwarg)
              val newStmt = AssignStmt(targets, Call(expr1, exprs, newkwds), ty)
              (newStmt :: optimWrapping, env.add("optimizer", idr))
            // positional learning rate scheduler
            case (Some(EName(ids)), None)
              if env.get("lr_scheduler") contains ids =>
                (stmt :: optimWrapping, env.add("optimizer", idr))
            // positional constant learning rate
            case (Some(h), None) =>
              val newExpr = parseExpr(s"(${beautify(h)}) * hvd.size()")
              val newExprs = newExpr :: exprs.tail
              val newStmt = AssignStmt(targets, Call(expr1, newExprs, kwds), ty)
              (newStmt :: optimWrapping, env.add("optimizer", idr))
            case _ =>
              val warning =
                Warning(
                  "No learning_rate",
                  ExprStmt(Call(expr1, exprs, kwds)))
              (stmt :: optimWrapping, env.add("optimizer", idr), warning)
          }

        // model.evaluate
        case Attribute(EName(idt), Id("evaluate")) if env.get("model") contains idt =>
          val verboseExpr = parseExpr("1 if hvd.rank() == 0 else 0")
          val newKwds = findKwarg(kwds, "verbose") match {
            case Some(vbKwarg) =>
              val newVbKwarg = vbKwarg.copy(expr = verboseExpr)
              replaceElement(kwds, vbKwarg, newVbKwarg)
            case None => kwds :+ NormalKwarg(Id("verbose"), verboseExpr)
          }
          val newStmt =
            AssignStmt(List(EName(idr)), Call(expr1, exprs, newKwds))
          (newStmt, env)
        case _ => super.transform(stmt)
      }

    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportStmt(alias) =>
      val classUpdatedEnv = transferStmt(env.getClassOrder)(stmt)
      val newEnv = transform(alias)(env.copy(classOrder = classUpdatedEnv))
      val diffEnv = newEnv \ env
      // get "tensor_flow" id
      diffEnv.get("tensor_flow") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 =>
          (List(ImportStmt(alias)) ++ getStmts("import-some", id), newEnv)
        // corresponding not found
        case _ => (ImportStmt(alias), newEnv)
      }

    /////////////////////////////////////////////////////////////////
    // strict form of expr stmts
    case ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      // model.compile
      case Attribute(EName(idt), Id("compile")) if env.get("model") contains idt =>
        val optim = Id("optim")
        findKwarg(kwds, "optimizer") match {
          case Some(kwarg) if kwarg.expr == EConst(StringLiteral("adam")) =>
            val newKwarg = kwarg.copy(expr = EName(optim))
            val newkwds = replaceElement(kwds, kwarg, newKwarg)
            val newStmts =  getStmts("assign-optimizer-default-adam", optim) ++
              ExprStmt(Call(expr1, exprs, newkwds))
            (newStmts, env)
          case None if exprs.headOption contains EConst(StringLiteral("adam")) =>
            val newStmts = getStmts("assign-optimizer-default-adam", optim) ++
              ExprStmt(Call(expr1, EName(optim) :: exprs.tail, kwds))
            (newStmts, env)
          case _ => super.transform(stmt)
        }

      // model.fit
      case Attribute(EName(idt), Id("fit")) if env.get("model") contains idt => 
        val verboseExpr = parseExpr("1 if hvd.rank() == 0 else 0")
        val callbacksExpr =
          parseExpr("[hvd.callbacks.BroadcastGlobalVariablesCallback(0)]")
        val vbKwds = findKwarg(kwds, "verbose") match {
          case Some(vbKwarg) =>
            val newVbKwarg = vbKwarg.copy(expr = verboseExpr)
            replaceElement(kwds, vbKwarg, newVbKwarg)
          case None => kwds :+ NormalKwarg(Id("verbose"), verboseExpr)
        }
        val speKwds = findKwarg(kwds, "steps_per_epoch") match {
          case Some(speKwarg) =>
            val newExpr = parseExpr(s"${beautify(speKwarg.expr)} // hvd.size()")
            val newSpeKwarg = speKwarg.copy(expr = newExpr)
            replaceElement(vbKwds, speKwarg, newSpeKwarg)
          case None =>
            // TODO add warning
            vbKwds
        }
        val newStmts = findKwarg(kwds, "callbacks") match {
          case Some(cbKwarg) =>
            val newCbKwarg = cbKwarg.copy(expr = EName(Id("callbacks")))
            val cbKwds = replaceElement(speKwds, cbKwarg, newCbKwarg)
            val init_callbacks = getStmts("init-callbacks", cbKwarg.expr)
            init_callbacks :+ ExprStmt(Call(expr1, exprs, cbKwds))
          case None =>
            val newCbKwarg = NormalKwarg(Id("callbacks"), callbacksExpr)
            ExprStmt(Call(expr1, exprs, speKwds :+ newCbKwarg)) :: Nil
        }
        (newStmts, env)
      case _ => super.transform(stmt)
    }
    case _ => super.transform(stmt)
  }

  override def getStmts(name:String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => super.getStmts(name, nodes)
    }

  private val codeData: Map[String, List[String] => String] = Map(
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
    "wrapping-optim" -> (codes => {
        val optim = codes(0)
        s"""$optim = hvd.DistributedOptimizer($optim)"""
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

package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.MainScriptRule
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object KerasRule extends KerasRule

// Transform rule for main module of keras model
trait KerasRule extends MainScriptRule {
  override def transform(stmt: Stmt)
  (implicit
    env: Env,
    isTopLevel: Boolean
  ): (List[Stmt], Env, List[Warning]) = stmt match {
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
    case ExprStmt(Call(expr1, exprs, kwds)) => {
      val call = Call(expr1, exprs, kwds)
      expr1 match {
        // model.compile
        // TODO: check
        case Attribute(EName(idt), Id("compile")) if env.get("model") contains idt =>
          val optim = Id("optim")
          findKwarg(kwds, "optimizer") match {
            // optimizer given as string literal
            case Some(kwarg) if kwarg.expr == EConst(StringLiteral("adam")) =>
              val newKwarg = kwarg.copy(expr = EName(optim))
              val newkwds = replaceElement(kwds, kwarg, newKwarg)
              val newStmts =  getStmts("assign-optimizer-default-adam", optim) ++
                ExprStmt(Call(expr1, exprs, newkwds))
              (newStmts, env)
            // optimizer given inline
            case Some(kwarg) => {
              kwarg.expr match {
                // 
                case optimExpr @ Call(callExpr, argExprs, kwdExprs) if env.isSubclass(callExpr, OPTIMIZER) => {
                  val lrScalingOptim: Expr => Expr = {
                    case e @ EName(ids) if env get "lr_scheduler" contains ids => e
                    case e => lrScaling(e)
                  }
                  val newOptimExpr = changeArg(optimExpr, "learning_rate", lrScalingOptim)  
                  val newOptimAssign: Stmt = AssignStmt(List(EName(optim)), newOptimExpr, None)
                  val newOptimWrapping: List[Stmt] = getStmts("wrapping-optim", optim)
                  val newCompileCall = changeArg(call, "optimizer", _ => EName(optim))
                  val newCompileStmt: Stmt = ExprStmt(newCompileCall)
                  val newStmtList = List(newOptimAssign) ++ newOptimWrapping ++ List(newCompileStmt)
                  (newStmtList, env)
                }
                // object not a optimizer object; error
                case _ => ???
              }
            }
            case None if exprs.headOption contains EConst(StringLiteral("adam")) =>
              val newStmts = getStmts("assign-optimizer-default-adam", optim) ++
                ExprStmt(Call(expr1, EName(optim) :: exprs.tail, kwds))
              (newStmts, env)
            case _ => super.transform(stmt)
          }

        // model.fit
        // TODO: Check
        case Attribute(EName(idt), Id("fit"))
          if env.get("model") contains idt => 
            val verboseExpr = parseExpr("1 if hvd.rank() == 0 else 0")
            val callbacksExpr =
              parseExpr("[hvd.callbacks.BroadcastGlobalVariablesCallback(0)]")
          val vbKwds = findKwarg(kwds, "verbose") match {
            case Some(vbKwarg) =>
              val newVbKwarg = vbKwarg.copy(expr = verboseExpr)
              replaceElement(kwds, vbKwarg, newVbKwarg)
            case None => kwds :+ NormalKwarg(Id("verbose"), verboseExpr)
          }
          val (speKwds, lw) = findKwarg(kwds, "steps_per_epoch") match {
            case Some(speKwarg) =>
              val newExpr = parseExpr(s"${beautify(speKwarg.expr)} // hvd.size()")
              val newSpeKwarg = speKwarg.copy(expr = newExpr)
              (replaceElement(vbKwds, speKwarg, newSpeKwarg), Nil)
            case None =>
              val warning =
                Warning("Epochs can be divided by `hvd.size()`", stmt)
              (vbKwds, List(warning))
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
          (newStmts, env, lw)
        case _ => super.transform(stmt)
      }
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
    "wrapping-optim" -> (codes => {
        val optim = codes(0)
        s"""$optim = hvd.DistributedOptimizer($optim)"""
    }),
  )
}

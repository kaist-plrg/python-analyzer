package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Preprocess._
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.transformer.Transformer
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object TransformerTape extends Transformer {
  def apply(module: Module): Module = module.copy(body=transform(module.body)(Env())._1)

  override def transform(stmt: Stmt)(implicit env: Env): (List[Stmt], Env) = stmt match {
    /////////////////////////////////////////////////////////////////
    //// strict form of assignment
    /////////////////////////////////////////////////////////////////
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        // case 1) "tensor_flow" -> data.Dataset
        case Attribute(Attribute(Attribute(EName(idt), Id("data")), Id("Dataset")), _)
          if env.get("tensor_flow") contains idt =>
            (AssignStmt(targets, Call(expr1, exprs, kwds), ty), 
              env.add("dataset", idr))

        // case 2) "tensor_flow" -> train.Checkpoint
        case Attribute(Attribute(EName(idt), Id("train")), Id("Checkpoint"))
          if env.get("tensor_flow") contains idt =>
            (AssignStmt(targets, Call(expr1, exprs, kwds), ty), 
              env.add("checkpoint", idr))

        // case 3) "tensor_flow" -> optimizers.Adam 
        case Attribute(Attribute(EName(idt), Id("optimizers")), Id("Adam"))
          if env.get("tensor_flow") contains idt =>
            // find id_i "learning_rate"
            findKwarg(kwds, "learning_rate") match {
              case Some(kwarg) =>
                val expr2i = kwarg.expr
                val newkwds = replaceElement(kwds, kwarg, kwarg.copy(
                  expr = parseExpr(s"(${beautify(expr2i)}) * hvd.size()")
                ))
                (AssignStmt(targets, Call(expr1, exprs, newkwds), ty), 
                  env.add("optimizer", idr))
              // such id_i doesn't exist
              case None =>
                val newexprs = 
                  List(parseExpr(s"(${beautify(exprs.head)}) * hvd.size()")) ++
                  exprs.tail
                (AssignStmt(targets, Call(expr1, newexprs, kwds), ty), 
                  env.add("optimizer", idr))
            }

        // case 3) "optimizer" -> apply_gradients
        case Attribute(EName(idt), Id("apply_gradients"))
          if env.get("optimizer") contains idt =>
            val idz = newId
            // find id_i "grads_and_vars"
            findKwarg(kwds, "grads_and_vars") match {
              case Some(kwarg) =>
                val expr2i = kwarg.expr
                val newkwds = replaceElement(kwds, kwarg,
                  kwarg.copy(expr = EName(idz)))
                val newStmts = List(
                  AssignStmt(List(EName(idz)), expr2i),
                  AssignStmt(targets, Call(expr1, exprs, newkwds), ty),
                ) ++ parseStmts(stmtData("assign-optimizer-some")(List(idz.name)))
                (newStmts, env) 
              // such id_i doesn't exist
              case None => 
                // idz == expr_11
                val newStmts = List(
                  AssignStmt(List(EName(idz)), exprs.head),
                  AssignStmt(targets, Call(expr1, EName(idz)::exprs.tail, kwds), ty)
                ) ++ parseStmts(stmtData("assign-optimizer-none")(List(idz.name)))
                (newStmts, env)
            }
        // case 4) "chcekpoint" -> idt.save
        case Attribute(EName(idt), Id("save"))
          if env.get("checkpoint") contains idt =>
            // hvd.rank()
            val rankExpr = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
            // hvd.rank() == 0
            val condExpr = CompExpr(rankExpr, List((CEq,EConst(IntLiteral(0)))))
            // if hvd.rank() == 0: ...
            val newIfStmt = IfStmt(condExpr, List(stmt), Nil)
            (newIfStmt, env)

        // case 5) etc.
        case _ =>
          super.transform(stmt)
          //(AssignStmt(targets, transform(Call(expr1, exprs, kwds)), ty), env)
      }
    // for `os.environ['CUDA_VISIBLE_DEVICES']` case
    case AssignStmt(
      List(Subscript(Attribute(EName(idt), Id("environ")), 
      EConst(StringLiteral("CUDA_VISIBLE_DEVICES")))), expr, ty)
      if env.get("os") contains idt =>
        (List(), env)
    // AnnAssign case: 
    case AnnAssign(e1, e2, e3) =>
      (e1, e3) match {
        // case 1) "tensor_flow" -> Dataset case
        case (EName(id1), 
              Some(Call(Attribute(Attribute(Attribute(EName(id2), Id("data")), Id("Dataset")), _), le, lk)))
          if env.get("tensor_flow") contains id2 => (
            AnnAssign(e1, e2, e3),
            env.add("dataset", id1)
          )
        // case 2) otherwise
        case _ => super.transform(stmt)
          //(AnnAssign(e1, e2, e3.map(transform)), env)
      }
    /////////////////////////////////////////////////////////////////
    // with statement
    /////////////////////////////////////////////////////////////////
    case WithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      // get "gradient_tape" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts = 
            List(WithStmt(ty, newItems, newStmts)) ++ 
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv)
        // not found
        case _ => (WithStmt(ty, newItems, newStmts), newEnv)
      }
    /////////////////////////////////////////////////////////////////
    // Async with statement
    /////////////////////////////////////////////////////////////////
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      // get "gradient_tap" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts =
            List(AsyncWithStmt(ty, newItems, newStmts)) ++
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv)
        case _ => (AsyncWithStmt(ty, newItems, newStmts), newEnv)
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
      // case 1) func expr is "optimizer.apply_gradients"
      case Attribute(EName(idt), Id("apply_gradients")) 
        if env.get("optimizer") contains idt =>
          val idz = newId
          // get "grads_and_vars" id
          findKwarg(kwds, "grads_and_vars") match {
            // found 
            case Some(kwarg) =>
              val expr2i = kwarg.expr
              val newkwarg = kwarg.copy(expr = EName(idz))
              val newkwds = replaceElement(kwds, kwarg, newkwarg)
              val newStmts = List(
                AssignStmt(List(EName(idz)), expr2i),
                ExprStmt(Call(expr1, exprs, newkwds)),
              ) ++ parseStmts(stmtData("expr-optimizer-some")(List(idz.name, idt.name)))

              (newStmts, env)
            // not found
            case None => 
              val newStmts = List(
                AssignStmt(List(EName(idz)), exprs.head),
                ExprStmt(Call(expr1, EName(idz) :: exprs.tail, kwds)),
              ) ++ parseStmts(stmtData("expr-optimizer-none")(List(idz.name, idt.name)))
              (newStmts, env)
          }
      // case 2) print stmt
      case EName(Id("print")) => {
        // hvd.rank()
        val rank = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
        // hvd.rank() == 0 
        val condExpr = CompExpr(rank, List((CEq,EConst(IntLiteral(0)))))
        // if hvd.rank() == 0: ...
        val ifStmt = IfStmt(condExpr, List(stmt), Nil)
        (List(ifStmt), env)
      }
      // case 3) "checkpoint"
      case Attribute(EName(idt), Id("save"))
        if env.get("checkpoint") contains idt =>
          // hvd.rank()
          val rank = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
          // hvd.rank() == 0 
          val condExpr = CompExpr(rank, List((CEq,EConst(IntLiteral(0)))))
          // if hvd.rank() == 0: ...
          val ifStmt = IfStmt(condExpr, List(stmt), Nil) 
          (ifStmt, env)
      // case _) other expr stmts
      case _ => (ExprStmt(transform(Call(expr1, exprs, kwds))), env)
    }
    case _ => super.transform(stmt)
  }

  /////////////////////////////////////////
  // transformer for Expression
  /////////////////////////////////////////
  override def transform(expr: Expr)(implicit env: Env): Expr = expr match {
    case Call(expr1, le, lk) => expr1 match {
      // case 1) σ("dataset") = id_t and expr_1 = id_t.take
      case Attribute(EName(idt), Id("take")) 
        if env.get("dataset") contains idt =>
          findKwarg(lk, "count") match {
            case Some(kwarg) =>
              val expr2i = kwarg.expr
              val newLk = replaceElement(lk, kwarg,
                kwarg.copy(expr = parseExpr(s"${beautify(expr2i)} // hvd.size()")))
              Call(expr1, le, newLk)
            case None => Call(
              expr1,
              parseExpr(s"${beautify(le.head)} // hvd.size()") :: le.tail,
              lk
            )
          }
      
      // case _) else
      case _ => super.transform(expr)
    }
    case _ => super.transform(expr)
  }

  override def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
    case WithItem(e, None) => (WithItem(transform(e), None), env)
    case WithItem(e, Some(asE)) => (env.get("tensor_flow"), e, asE) match {
      case (
        Some(x),
        Call(Attribute(EName(f), Id("GradientTape")), Nil, Nil),
        EName(asX)
      ) if x == f =>
          (WithItem(e, Some(asE)), env.add("gradient_tape", asX))
      case _ => (WithItem(transform(e), Some(asE)), env)
      }
  }

  /////////////////////////////////////////
  // Data needed for transformation
  // TODO this is actually static thingy...
  /////////////////////////////////////////
  val stmtData: Map[String, List[String] => String] = Map(
    // strict assign
    "assign-optimizer-some" -> (names => {
        val name = names(0)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${name}],
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // strict assign
    "assign-optimizer-none" -> (names => { 
        val name = names(0)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${name}],
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // import stmt
    "import-some" -> (names => { 
        val name = names(0)
        s"""import horovod.tensorflow as hvd
           |hvd_broadcast_done = False
           |hvd.init()
           |gpus = ${name}.config.experimental.list_physical_devices('GPU')
           |for gpu in gpus:
           |  ${name}.config.experimental.set_memory_growth(gpu, True)
           |if gpus:
           |  ${name}.config.experimental.\\
           |    set_visible_devices(gpus[hvd.local_rank()], 'GPU')""".stripMargin
    }),
    // expression stmt
    "expr-optimizer-some" -> (names => {
        val idz = names(0)
        val idt = names(1)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${idz}],
           |    root_rank=0
           |  )
           |  hvd.broadcast_variables(
           |    ${idt}.variables(),
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // expression stmt
    "expr-optimizer-none" -> (names => { 
        val idz = names(0)
        val idt = names(1)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${idz}],
           |    root_rank=0
           |  )
           |  hvd.broadcast_variables(
           |    ${idt}.variables(),
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
  )
}
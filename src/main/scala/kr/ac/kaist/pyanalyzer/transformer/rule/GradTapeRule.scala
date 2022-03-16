package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.MainScriptRule
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object GradTapeRule extends GradTapeRule

// Transform rule for main module of GradientTape model
trait GradTapeRule extends MainScriptRule {
  override def transform(stmt: Stmt)
  (implicit
    env: Env,
    isTopLevel: Boolean
  ): (List[Stmt], Env, List[Warning]) = stmt match {
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
                ) ++ getStmts("assign-optimizer-some", idz)
                (newStmts, env) 
              // such id_i doesn't exist
              case None => 
                // idz == expr_11
                val newStmts = List(
                  AssignStmt(List(EName(idz)), exprs.head),
                  AssignStmt(targets, Call(expr1, EName(idz)::exprs.tail, kwds), ty)
                ) ++ getStmts("assign-optimizer-none", idz)
                (newStmts, env)
            }

        case _ =>
          super.transform(stmt)
          //(AssignStmt(targets, transform(Call(expr1, exprs, kwds)), ty), env)
      }

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

    case stmt @ ForStmt(ty, forExpr,
      Call(
        EName(Id("range")),
        List(Attribute(EName(config), Id("iterations_per_epoch"))),
        Nil
      ), doStmt, elseStmt) if env.get("config") contains config =>
        val newCallExpr = parseExpr(
          s"range(${config.name}.iterations_per_epoch // hvd.size()")
        val (newDoStmt, _, dolw) = transform(doStmt)
        val (newElseStmt, _, elselw) = transform(elseStmt)
        val newForStmt =
          ForStmt(ty, forExpr, newCallExpr, newDoStmt, newElseStmt)
        (newForStmt, env, dolw ++ elselw)
    case stmt @ ForStmt(ty, forExpr,
      Call(EName(Id("tqdm")), List(
        Call(
          EName(Id("range")),
          List(Attribute(EName(config), Id("iterations_per_epoch"))),
          Nil
        )
      ), Nil), doStmt, elseStmt) if env.get("config") contains config =>
        val newCallExpr = parseExpr(
          s"range(${config.name}.iterations_per_epoch // hvd.size())")
        val (newDoStmt, _, dolw) = transform(doStmt)
        val (newElseStmt, _, elselw) = transform(elseStmt)
        val newForStmt =
          ForStmt(ty, forExpr, newCallExpr, newDoStmt, newElseStmt)
        (newForStmt, env, dolw ++ elselw)
    /////////////////////////////////////////////////////////////////
    // with statement
    /////////////////////////////////////////////////////////////////
    case WithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv, lw) = transform(doStmt)(tempEnv, isTopLevel)
      val diffEnv = tempEnv \ env
      // get "gradient_tape" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts = 
            List(WithStmt(ty, newItems, newStmts)) ++ 
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv, lw)
        // not found
        case _ => (WithStmt(ty, newItems, newStmts), newEnv, lw)
      }
    /////////////////////////////////////////////////////////////////
    // Async with statement
    /////////////////////////////////////////////////////////////////
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv, lw) = transform(doStmt)(tempEnv, isTopLevel)
      val diffEnv = tempEnv \ env
      // get "gradient_tap" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts =
            List(AsyncWithStmt(ty, newItems, newStmts)) ++
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv, lw)
        case _ => (AsyncWithStmt(ty, newItems, newStmts), newEnv, lw)
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
          val newStmts = List(ImportStmt(alias)) ++ getStmts("import-some", id) 
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
              ) ++ (
                if (isTopLevel) getStmts("top-level-expr-optimizer-some", idz, idt)
                else getStmts("expr-optimizer-some", idz, idt)
              )

              (newStmts, env)
            // not found
            case None => 
              val newStmts = List(
                AssignStmt(List(EName(idz)), exprs.head),
                ExprStmt(Call(expr1, EName(idz) :: exprs.tail, kwds)),
              ) ++ (
                if (isTopLevel) getStmts("top-level-expr-optimizer-none", idz, idt)
                else getStmts("expr-optimizer-none", idz, idt)
              )
              (newStmts, env)
          }
      // case _) other expr stmts
      case _ => super.transform(stmt)
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

  override def transform(alias: Alias)(implicit env: Env): Env = alias match {
    case Alias(List(x), None) if x.name == "config" =>
      env.add("config", x)
    case Alias(List(x), Some(as)) if x.name == "config" =>
      env.add("config", as)
    case _ => super.transform(alias)
  }

  override def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
    case WithItem(e, Some(EName(as))) => e match {
      case Call(Attribute(EName(tf), Id("GradientTape")), Nil, Nil)
      if env.get("tensor_flow") contains tf =>
          (WithItem(e, Some(EName(as))), env.add("gradient_tape", as))
      case _ => (WithItem(transform(e), Some(EName(as))), env)
    }
    case WithItem(e, opt) => (WithItem(transform(e), opt), env)
  }

  override def getStmts(name:String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => super.getStmts(name, nodes)
    }

  private val codeData: Map[String, List[String] => String] = Map(
    // no optim wrapping in DistGradTape
    "wrapping-optim" -> (codes => {
      ""
    }),
    // strict assign
    "assign-optimizer-some" -> (names => {
        val name = names(0)
        s"""if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${name}],
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // strict assign
    "assign-optimizer-none" -> (names => { 
        val name = names(0)
        s"""if not hvd_broadcast_done:
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
    "top-level-expr-optimizer-some" -> (names => {
        val idz = names(0)
        val idt = names(1)
        s"""if not hvd_broadcast_done:
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
    "top-level-expr-optimizer-none" -> (names => { 
        val idz = names(0)
        val idt = names(1)
        s"""if not hvd_broadcast_done:
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

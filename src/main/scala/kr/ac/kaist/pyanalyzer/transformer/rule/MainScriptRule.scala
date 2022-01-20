package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Useful._

// Transform rule for main module
trait MainScriptRule extends Transformer {
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case stmt @ AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      val call = Call(expr1, exprs, kwds)
      expr1 match {
        // learning rate schelduler
        case _ if env.isSubclass(expr1, CONST_LEARNING_RATE_SCHEDULER) =>
          (stmt, env.add("lr_scheduler", idr))
        case _ if env.isSubclass(expr1, LEARNING_RATE_SCHEDULER) =>
          val newCall =
            changeArg(call, "initial_learning_rate", lrScaling)
          val newStmt = stmt.copy(expr=newCall)
          (newStmt, env.add("lr_scheduler", idr))
        case _ if env.isSubclass(expr1, LEARNING_RATE_SCHEDULER_TF_V1) =>
          val newCall =
            changeArg(call, "learning_rate", lrScaling)
          val newStmt = stmt.copy(expr=newCall)
          (newStmt, env.add("lr_scheduler", idr))

        // optimizer
        case _ if env.isSubclass(expr1, OPTIMIZER) =>
          val optimWrapping = getStmts("wrapping-optim", idr)
          val lrScalingOptim: Expr => Expr = {
            case e @ EName(ids) if env get "lr_scheduler" contains ids => e
            case e => lrScaling(e)
          }
          val newCall =
            changeArg(call, "learning_rate", lrScalingOptim)
          val newStmt = stmt.copy(expr=newCall)
          (newStmt :: optimWrapping, env.add("optimizer", idr))

        // model
        case _ if env.isSubclass(expr1, MODEL) =>
          (stmt, env.add("model", idr))
        case Attribute(EName(idt), Id("evaluate"))
          if env.get("model") contains idt =>
            val verboseExpr = parseExpr("1 if hvd.rank() == 0 else 0")
            val newCall =
              changeOrAddArg(call, "verbose", verboseExpr, 3)
            val newStmt = stmt.copy(expr=newCall)
            (newStmt, env)

        // check point
        case Attribute(Attribute(EName(idt), Id("train")), Id("Checkpoint"))
          if env get "tensor_flow" contains idt =>
            (stmt, env.add("checkpoint", idr))

        case _ => super.transform(stmt)
      }

    // TODO: check
    case stmt @ ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      // removing gpu pinning code
      // tf.config.experimental.set_visible_devices
      case Attribute(Attribute(
        Attribute(EName(tf), Id("config")),
        Id("experimental")
      ), Id("set_visible_devices"))
        if env.get("tensor_flow") contains tf =>
          (Nil, env)
      case Attribute(EName(idt), id)
        if env.get("model").contains(idt) && WRITE_METHOD.contains(id.name) =>
          (getStmts("root-rank-blocking", stmt), env)
      case Attribute(EName(idt), Id("save"))
        if env.get("checkpoint") contains idt =>
          (getStmts("root-rank-blocking", stmt), env)
      case Attribute(receiver, id) if WRITE_METHOD contains id.name =>
        val warningMessage =
          s"""Cannot identify the receiver, `${beautify(receiver)}`
          |    Root rank blocking to this statement can be inaccurate"""
          .stripMargin
        val warning =
          Warning(warningMessage, stmt)
        (getStmts("root-rank-blocking", stmt), env, warning)
      case EName(Id("print")) =>
        (getStmts("root-rank-blocking", stmt), env)
      case Attribute(EName(idt), Id("print"))
      if env.get("tensor_flow") contains idt =>
        (getStmts("root-rank-blocking", stmt), env)
      case _ => (ExprStmt(super.transform(Call(expr1, exprs, kwds))), env)
    }
    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportFromStmt(lv, fromId, al) =>
      val classUpdatedEnv = transferStmt(env.getClassOrder)(stmt)
      val newEnv = transform(al)(env.copy(classOrder = classUpdatedEnv))
      (ImportFromStmt(lv, fromId, al), newEnv)
    case _ =>
      val newEnv = env.copy(classOrder = transferStmt(env.getClassOrder)(stmt))
      super.transform(stmt)(newEnv)
  }

  override def transform(expr: Expr)(implicit env: Env): Expr = expr match {
    case Call(EName(tqdm), List(e), Nil)
      if env.get("tqdm") contains tqdm => e
    case _ => super.transform(expr)
  }

  override def transform(alias: Alias)(implicit env: Env): Env = alias match {
    case Alias(List(x), None) if x.name == "tqdm" =>
      env.add("tqdm", x)
    case Alias(List(x), Some(as)) if x.name == "tqdm" =>
      env.add("tqdm", as)
    case Alias(List(tf, compat, v1), None)
    if tf.name == "tensorflow" && compat.name == "compat" && v1.name == "v1" =>
      ???
    case Alias(List(tf, compat, v1), Some(as))
    if tf.name == "tensorflow" && compat.name == "compat" && v1.name == "v1" =>
      env.add("tensor_flow_v1", as)
    case Alias(List(x), None) if x.name == "tensorflow" =>
      env.add("tensor_flow", x)
    case Alias(List(x), Some(as)) if x.name == "tensorflow" =>
      env.add("tensor_flow", as)
    case Alias(List(x), None) if x.name == "keras" =>
      env.add("keras", x)
    case Alias(List(x), Some(as)) if x.name == "keras" =>
      env.add("keras", as)
    case Alias(List(x), None) if x.name == "optimizers" =>
      env.add("optimizers", x)
    case Alias(List(x), Some(as)) if x.name == "optimizers" =>
      env.add("optimizers", as)
    case _ => super.transform(alias)
  }

  override def getStmts(name:String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => super.getStmts(name, nodes)
    }
  private val codeData: Map[String, List[String] => String] = Map(
    "import-some" -> (codeSeg => {
      val tf = codeSeg(0)
      s"""import horovod.tensorflow as hvd
          |hvd.init()""".stripMargin
    }),
    "wrapping-optim" -> (codes => {
        val optim = codes(0)
        s"""$optim = hvd.DistributedOptimizer($optim)"""
    }),
    "root-rank-blocking" -> (codeSeg => {
      val stmt = codeSeg(0)
      s"""if hvd.rank() == 0: $stmt"""
    }),
  )
}


// Transform rule for main module
trait TFv1MainScriptRule extends MainScriptRule {

  // TODO: check
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        case Attribute(EName(idt), Id("ConfigProto"))
        if env.get("tensor_flow_v1") contains idt =>
          (stmt :: getStmts("config-exist", idt), env.add("config_proto", idr))

        case Attribute(Call(expr2, innerExprs, innerKwds), Id("minimize"))
        if env.isSubclass(expr2, OPTIMIZER_TF_V1) =>
          val minimizeCall = Call(EName(Id("minimize")), exprs, kwds)
          val optimWrapping =
            getStmts("wrapping-optim-minimize", idr, minimizeCall)
          // assume learning_rate is first positional argument
          (innerExprs.headOption, findKwarg(innerKwds, "learning_rate")) match {
            // keyword learning rate scheduler
            case (None, Some(NormalKwarg(_, EName(ids))))
              if env.get("lr_scheduler") contains ids =>
                val newStmt =
                  AssignStmt(targets, Call(expr2, innerExprs, innerKwds), ty)
                (newStmt :: optimWrapping, env.add("optimizer", idr))
            // keyword constant learning rate
            case (None, Some(kwarg)) =>
              val newExpr = parseExpr(s"(${beautify(kwarg.expr)}) * hvd.size()")
              val newKwarg = kwarg.copy(expr = newExpr)
              val newkwds = replaceElement(innerKwds, kwarg, newKwarg)
              val newStmt =
                AssignStmt(targets, Call(expr2, innerExprs, newkwds), ty)
              (newStmt :: optimWrapping, env.add("optimizer", idr))
            // positional learning rate scheduler
            case (Some(EName(ids)), None)
              if env.get("lr_scheduler") contains ids =>
                val newStmt =
                  AssignStmt(targets, Call(expr2, innerExprs, innerKwds), ty)
                (newStmt :: optimWrapping, env.add("optimizer", idr))
            // positional constant learning rate
            case (Some(h), None) =>
              val newExpr = parseExpr(s"(${beautify(h)}) * hvd.size()")
              val newExprs = newExpr :: innerExprs.tail
              val newStmt =
                AssignStmt(targets, Call(expr2, newExprs, innerKwds), ty)
              (newStmt :: optimWrapping, env.add("optimizer", idr))
            case _ =>
              val warning =
                Warning("Cannot find learning_rate", stmt)
              val newStmt =
                AssignStmt(targets, Call(expr2, innerExprs, innerKwds), ty)
              (newStmt :: optimWrapping, env.add("optimizer", idr), warning)
          }
        case _ => super.transform(stmt)
      }

    // TODO: check
    case ImportStmt(alias) =>
      val classUpdatedEnv = transferStmt(env.getClassOrder)(stmt)
      val newEnv = transform(alias)(env.copy(classOrder = classUpdatedEnv))
      val diffEnv = newEnv \ env
      diffEnv.get("tensor_flow_v1") match {
        case Some(id) if diffEnv.size == 1 =>
          (stmt :: getStmts("import-some", id), newEnv)
        case _ => (ImportStmt(alias), newEnv)
      }

    case _ => super.transform(stmt)
  }

  override def getStmts(name:String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => super.getStmts(name, nodes)
    }

  private val codeData: Map[String, List[String] => String] = Map(
    "config-exist" -> (codeSeg => {
      val config = codeSeg(0)
      s"""config.gpu_options.visible_device_list = str(hvd.local_rank())""".stripMargin
    }),
    "config-none" -> (codeSeg => {
      val tf = codeSeg(0)
      s"""config = $tf.ConfigProto()
          |config.gpu_options.allow_growth = True
          |config.gpu_options.visible_device_list = str(hvd.local_rank())""".stripMargin
    }),
    "wrapping-optim-minimize" -> (codes => {
        val optim = codes(0)
        val minimize = codes(1)
        s"""$optim = hvd.DistributedOptimizer($optim).$minimize"""
    }),
  )
}

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
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        case _ if env.isSubclass(expr1, LEARNING_RATE_SCHEDULER) =>
          findKwarg(kwds, "initial_learning_rate") match {
            // keword initial learning rate
            case Some(kwarg) =>
              val newExpr = parseExpr(s"${beautify(kwarg.expr)} * hvd.size()")
              val newKwarg = kwarg.copy(expr = newExpr)
              val newKwds = replaceElement(kwds, kwarg, newKwarg)
              val newStmt = AssignStmt(targets, Call(expr1, exprs, newKwds), ty)
              (newStmt, env.add("lr_scheduler", idr))
            // no initial learning rate
            case None
              if env.isSubclass(expr1,CONST_LEARNING_RATE_SCHEDULER) =>
                (stmt, env.add("lr_scheduler", idr))
            case None => exprs match {
              // positional initial learning rate
              case h :: t =>
                val newExprs = parseExpr(s"${beautify(h)} * hvd.size()") :: t
                val newStmt = AssignStmt(targets, Call(expr1, newExprs, kwds), ty)
                (newStmt, env.add("lr_scheduler", idr))
              case Nil =>
                val warning =
                  Warning("Cannot find initial_learning_rate", stmt)
                (stmt, env.add("lr_scheduler", idr), warning)
            }
          }

        case _ if env.isSubclass(expr1, MODEL) =>
          (stmt, env.add("model", idr))
        case Attribute(Attribute(EName(idt), Id("train")), Id("Checkpoint"))
          if env.get("tensor_flow") contains idt =>
            (stmt, env.add("checkpoint", idr))
        case _ => super.transform(stmt)
      }
    case stmt @ ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      case Attribute(EName(idt), id)
        if env.get("model").contains(idt) && WRITE_METHOD.contains(id.name) =>
          (getStmts("root-rank-blocking", stmt), env)
      case Attribute(EName(idt), Id("save"))
        if env.get("checkpoint") contains idt =>
          (getStmts("root-rank-blocking", stmt), env)
      case Attribute(receiver, id) if WRITE_METHOD contains id.name =>
        val warningMessage =
          s"""Cannot identify the receiver, `${beautify(receiver)}`
          |    Root rank blocking to this statement can be inaccurate""".stripMargin
        val warning =
          Warning(warningMessage, stmt)
        (getStmts("root-rank-blocking", stmt), env, warning)
      case EName(Id("print")) =>
        (getStmts("root-rank-blocking", stmt), env)
      case Attribute(EName(idt), Id("print")) if env.get("tensor_flow") contains idt =>
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

  override def transform(alias: Alias)(implicit env: Env): Env = alias match {
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
    "root-rank-blocking" -> (codeSeg => {
      val stmt = codeSeg(0)
      s"""if hvd.rank() == 0: $stmt"""
    }),
  )
}


// Transform rule for main module
trait TFv1MainScriptRule extends MainScriptRule {

  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        case Attribute(EName(idt), Id("ConfigProto"))
        if env.get("tensor_flow_v1") contains idt =>
          (stmt :: getStmts("config-exist", idt), env.add("config", idr))
        case _ => super.transform(stmt)
      }

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
    "import-some" -> (codeSeg => {
      val tf = codeSeg(0)
      s"""import horovod.tensorflow as hvd
          |hvd.init()""".stripMargin
    }),
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
  )
}

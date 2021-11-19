package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.MainScriptRule
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object MonSessRule extends MonSessRule {
  def apply(module: Module)(implicit env: Env = Env()): (Module, List[Warning]) = {
    val (stmts, _, lw) = transform(module.body)
    (module.copy(body=stmts), lw)
  }
}

// Transform rule for main module of MonitoredSession model
trait MonSessRule extends TFv1MainScriptRule {
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case WithStmt(ty, items, doStmt) if !env.contains("config_proto") =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv, lw) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      diffEnv.get("monitored_session") match {
        case Some(id) if diffEnv.size == 1 =>
          val transStmts =
            getStmts("config-none", id) :+ WithStmt(ty, newItems, newStmts)
          (transStmts, newEnv, lw)
        case _ => (WithStmt(ty, newItems, newStmts), newEnv, lw)
      }
    case _ => super.transform(stmt)
  }

  override def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
    case WithItem(e, Some(EName(as))) => e match {
      case Call(
        Attribute(
          Attribute(EName(tf), Id("train")),
          Id("MonitoredTrainingSession")
        ), exprs, kwds)
        if env.get("tensor_flow_v1").contains(tf) =>
          // hook
          val hookKwds = findKwarg(kwds, "hooks") match {
            case Some(kwarg) =>
              val beautified = beautify(kwarg.expr)
              val newExpr = parseExpr(
                s"$beautified.append(hvd.BroadcastGlobalVariablesHook(0))")
              replaceElement(kwds, kwarg, kwarg.copy(expr=newExpr))
            case None =>
              val newExpr =
                parseExpr("[hvd.BroadcastGlobalVariablesHook(0)]")
              val newKwarg = NormalKwarg(Id("hooks"), newExpr)
              kwds :+ newKwarg
          }
          // checkpoint
          val cpKwds = findKwarg(hookKwds, "checkpoint_dir") match {
            case Some(kwarg) =>
              val beautified = beautify(kwarg.expr)
              val newExpr =
                parseExpr(s"$beautified if hvd.rank() == 0 else None")
              replaceElement(hookKwds, kwarg, kwarg.copy(expr=newExpr))
            case None => hookKwds
          }

          // config
          val configKwarg = NormalKwarg(Id("config"), EName(Id("config")))
          val newKwds =
            if (env contains "config_proto") cpKwds
            else cpKwds :+ configKwarg

          // transformed result
          val newCallExpr = Call(
            Attribute(
              Attribute(EName(tf), Id("train")),
              Id("MonitoredTrainingSession")
            ),exprs, newKwds)
          val newWithStmt = WithItem(newCallExpr, Some(EName(as)))
          (newWithStmt, env.add("monitored_session", as))
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
  )
}

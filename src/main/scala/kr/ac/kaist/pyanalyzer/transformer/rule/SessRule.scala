package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.MainScriptRule
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object SessRule extends SessRule {
  def apply(module: Module)(implicit env: Env = Env()): (Module, List[Warning]) = {
    val (stmts, _, lw) = transform(module.body)
    (module.copy(body=stmts), lw)
  }
}

// Transform rule for main module of Session model
trait SessRule extends TFv1MainScriptRule {
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case ExprStmt(
      Call(
        Attribute(EName(sess), Id("run")),
        List(Call(Attribute(
          EName(tf), Id("global_variables_initializer")
        ), Nil, Nil)),
        Nil
      )) if env.get("session").contains(sess) &&
        env.get("tensor_flow_v1").contains(tf) =>
          (stmt :: getStmts("broadcast-in-sess"), env)
    case ExprStmt(
      Call(
        Attribute(Call(Attribute(
          EName(tf), Id("global_variables_initializer")
        ), Nil, Nil),Id("run")),
        Nil,
        Nil
      )) if env.get("tensor_flow_v1") contains tf =>
          (stmt :: getStmts("broadcast-outside-sess"), env)
    case WithStmt(ty, items, doStmt) if !env.contains("config_proto") =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv, lw) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      diffEnv.get("session") match {
        case Some(id) if diffEnv.size == 1 =>
          val transStmts =
            getStmts("config-none", env("tensor_flow_v1")) :+ WithStmt(ty, newItems, newStmts)
          (transStmts, newEnv, lw)
        case _ => (WithStmt(ty, newItems, newStmts), newEnv, lw)
      }
    case _ => super.transform(stmt)
  }

  override def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
    case WithItem(e, Some(EName(as))) => e match {
      case Call(Attribute(EName(tf), Id("Session")), exprs, kwds)
      if env.get("tensor_flow_v1").contains(tf) &&
      !env.contains("config_proto") =>
        val newKwarg = NormalKwarg(Id("config"), EName(Id("config")))
        val newExpr =
          Call(Attribute(EName(tf), Id("Session")), exprs, kwds :+ newKwarg)
        (WithItem(newExpr, Some(EName(as))), env.add("session", as))
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
    "broadcast-in-sess" -> (codes => {
      s"""sess.run(hvd.broadcast_global_variables(root_rank=0))"""
    }),
    "broadcast-outside-sess" -> (codes => {
      s"""hvd.broadcast_global_variables(root_rank=0).run()"""
    }),
  )
}

package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.transformer.MainScriptRule
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object EstRule extends EstRule {
  def apply(module: Module)(implicit env: Env = Env()): (Module, List[Warning]) = {
    val (stmts, _, lw) = transform(module.body)
    (module.copy(body=stmts), lw)
  }
}

// Transform rule for main module of Estimator model
trait EstRule extends TFv1MainScriptRule {
  override def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) =>
      val targets = List(EName(idr))
      expr1 match {
        case Attribute(Attribute(EName(tf), Id("estimator")), Id("Estimator"))
        if env.get("tensor_flow_v1").contains(tf) && !env.contains("config") =>
          val newKwarg = NormalKwarg(Id("config"), EName(Id("config")))
          val newStmt = AssignStmt(targets, Call(expr1, exprs, kwds :+ newKwarg), ty)
          val newStmts = getStmts("config-none", tf) :+ newStmt
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
  )
}

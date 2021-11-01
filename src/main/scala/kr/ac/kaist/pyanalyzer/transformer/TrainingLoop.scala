package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.util.Errors._
import kr.ac.kaist.pyanalyzer.util.UnitWalker
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object TrainingLoop {
  def apply(module: Module, order: ClassOrder): ModuleSummary = {
    val (env, tl) = getBodySummary(order = order, body = module.body)
    ModuleSummary(module.name, env, tl)
  }

  private def getBodySummary(
    outerEnv: TLEnv[Summary] = TLEnv[Summary](),
    order: ClassOrder,
    body: List[Stmt]
  ): (TLEnv[Summary], TLType) = {
    // variables for storing the side effect of UnitWalker
    implicit var newOrder = order
    var env = TLEnv[Summary]()
    var tl: TLType = Bot

    // UnitWalker for summary
    object SummaryWalker extends SummaryWalker
    trait SummaryWalker extends UnitWalker {

      // update summary map
      override def walk(stmt: Stmt): Unit = stmt match {
        case ImportStmt(_) => newOrder = transferStmt(newOrder)(stmt)

        case ImportFromStmt(_, _, _) => newOrder = transferStmt(newOrder)(stmt)

        case FunDef(_, x, _, _, _, body) =>
          val (_, innerTl) = getBodySummary(outerEnv ++ env, newOrder, body)
          env += x -> FuncSummary(x.name, innerTl)

        case AsyncFunDef(_, x, _, _, _, body) =>
          val (_, innerTl) = getBodySummary(outerEnv ++ env, newOrder, body)
          env += x -> FuncSummary(x.name, innerTl)

        case AssignStmt(List(EName(x)), Call(expr1, _, _), _)
          if isSubclass(expr1, MODEL) =>
            env += x -> ValueSummary(x.name, "model")

        case _ =>
          newOrder = transferStmt(newOrder)(stmt)
          super.walk(stmt)
      }

      // set tl
      override def walk(expr: Expr): Unit = expr match {
        // DistributedGradientTape training loop identifier
        case Call(expr1, _, _) if isSubclass(expr1, "tensorflow.GradientTape") =>
            // multiple training loops
            if (tl == Optimizer) throw TLException
            tl = GradTape

        // DistributedOptimizer training loop identifier
        case Call(Attribute(EName(model), Id("fit")), _, _)
          if (outerEnv ++ env).get(model) contains ValueSummary(model.name, "model") =>
            // multiple training loops
            if (tl == GradTape) throw TLException
            tl = Optimizer

        // Normal Call expression
        case Call(EName(x), _, _) =>
          (outerEnv ++ env).get(x) match {
            case Some(FuncSummary(x.name, innerTl)) if innerTl != Bot =>
              tl = innerTl
            // TODO: Add ClassSummary case
            case _ => super.walk(expr)
          }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    (env, tl)
  }

  def isSubclass(
    expr: Expr,
    parentCandidates: List[String]
  )(implicit order: ClassOrder): Boolean =
    parentCandidates.exists(isSubclass(expr, _))
  def isSubclass(
    expr: Expr,
    parentName: String
  )(implicit order: ClassOrder): Boolean =
    order.parseFullname(expr) match {
      case Some(fullname) =>
        val parentFullname = parseStrFullname(parentName)
        if (fullname == parentFullname) true
        else order.safeIsSubclass(fullname, parentFullname)
      case None => false
    }
}

case class TLEnv[T <: Summary](env: Map[Id, T] = Map[Id, T]()) {
  override def toString: String = toString(0)
  def toString(depth: Int): String = {
    val indent = "  " * depth
    env.foldLeft("") {
      case (str, (_, summary)) =>
        s"$str$indent${summary.toString(depth + 1)}"
    }
  }
  def apply(key: Id): T = env(key)
  def +(p: (Id, T)): TLEnv[T] = TLEnv[T](env + p)
  def ++(rhs: TLEnv[T]): TLEnv[T] = TLEnv[T](env ++ rhs.env)
  def get(key: Id): Option[T] = env.get(key)
  def foldLeft[B](z: B)(op: (B, (Id, T)) => B): B = env.foldLeft(z)(op)
  def contains(key: Id): Boolean = env contains key
  def find(p: ((Id, T)) => Boolean): Option[(Id, T)] = env find p
}

abstract class Summary {
  override def toString: String = toString(1)
  def toString(depth: Int): String = this match {
    case ModuleSummary(name, env, tl) if depth == 1 =>
      s"└ M-$name: $tl\n${env.toString(depth)}"
    case ModuleSummary(name, env, tl) =>
      s"• M-$name: $tl\n${env.toString(depth)}"
    case FuncSummary(name, tl) =>
      s"• F-$name: $tl\n"
    case ValueSummary(name, str) => s"• V-$name: $str\n"
  }
}

case class ModuleSummary(
  name: String,
  env: TLEnv[Summary],
  tl: TLType,
) extends Summary

case class FuncSummary(
  name: String,
  tl: TLType,
) extends Summary

case class ValueSummary(
  name: String,
  value: String
) extends Summary

trait TLType

case object GradTape extends TLType

case object Optimizer extends TLType

case object Bot extends TLType

trait SumType

case object TLModule extends SumType

case object TLFunc extends SumType

case object TLClass extends SumType

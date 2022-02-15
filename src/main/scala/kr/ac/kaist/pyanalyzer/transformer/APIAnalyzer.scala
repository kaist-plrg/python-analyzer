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

object APIAnalyzer {
  def apply(module: Module, order: ClassOrder): ModuleSummary = {
    val (env, api, _) = getBodySummary(order = order, body = module.body)
    ModuleSummary(module.name, env, api)
  }

  private def getBodySummary(
    outerEnv: SummaryEnv[Summary] = SummaryEnv[Summary](),
    order: ClassOrder,
    body: List[Stmt]
  ): (SummaryEnv[Summary], APIType, Option[ValueSummary]) = {
    // variables for storing the side effect of UnitWalker
    implicit var newOrder = order
    var env = SummaryEnv[Summary]()
    var api: APIType = Bot
    var retSummary: Option[ValueSummary] = None

    // UnitWalker for summary
    object SummaryWalker extends SummaryWalker
    trait SummaryWalker extends UnitWalker {

      // update summary map
      override def walk(stmt: Stmt): Unit = stmt match {
        case ImportStmt(_) => newOrder = transferStmt(newOrder)(stmt)

        case ImportFromStmt(_, _, _) => newOrder = transferStmt(newOrder)(stmt)

        case FunDef(_, x, _, _, _, body) =>
          val (_, innerAPI, innerRet) = getBodySummary(outerEnv ++ env, newOrder, body)
          env += x -> FuncSummary(x.name, innerAPI, innerRet)

        case AsyncFunDef(_, x, _, _, _, body) =>
          val (_, innerAPI, innerRet) = getBodySummary(outerEnv ++ env, newOrder, body)
          env += x -> FuncSummary(x.name, innerAPI, innerRet)

        case AssignStmt(List(EName(x)), Call(expr1, _, _), _)
          if isSubclass(expr1, MODEL) =>
            env += x -> ValueSummary(x.name, "model")

        case AssignStmt(List(EName(x)), Call(EName(f), _, _), _)
          if (env ++ outerEnv) contains f => (env ++ outerEnv)(f) match {
            case FuncSummary(_, innerAPI, opt) =>
              api = api ⊔ innerAPI
              opt.map(env += x -> _.copy(name=x.name))
            case ValueSummary(name, v) =>
            case _ => ???
          }

        case ReturnStmt(Some(Call(e, _, _))) if isSubclass(e, MODEL) =>
          retSummary = Some(ValueSummary("", "model"))

        // TODO
        case ReturnStmt(Some(Call(Call(e, _, _), _, _))) if isSubclass(e, MODEL) =>
          retSummary = Some(ValueSummary("", "model"))

        case _ =>
          newOrder = transferStmt(newOrder)(stmt)
          super.walk(stmt)
      }

      // set api
      override def walk(expr: Expr): Unit = expr match {
        // Session API indicator
        case Call(expr1, _, _)
        if isSubclass(expr1, "tensorflow.compat.v1.Session") =>
          if (!(api ⊑ Sess)) throw APIException
          api = Sess

        // MonitoredSession API indicator
        case Call(expr1, _, _)
        if isSubclass(expr1, "tensorflow.compat.v1.train.MonitoredTrainingSession") =>
          if (!(api ⊑ MonSess)) throw APIException
          api = MonSess

        // Estimator API indicator
        case Call(expr1, _, _)
        if isSubclass(expr1, "tensorflow.compat.v1.estimator.Estimator") =>
          if (!(api ⊑ Est)) throw APIException
          api = Est

        // TODO: Simple-CNN-MNIST-2 not detected!
        // GradientTape API indicator
        case Call(expr1, _, _) if isSubclass(expr1, "tensorflow.GradientTape") =>
          if (!(api ⊑ GradTape)) throw APIException
          api = GradTape

        // Keras API indicator
        case Call(Attribute(EName(model), Id("fit")), _, _)
        if (outerEnv ++ env).get(model) contains ValueSummary(model.name, "model") =>
          if (!(api ⊑ Keras)) throw APIException
          api = Keras

        case Call(expr1, _, _)
        if isSubclass(expr1, "tensorflow.compat.v1.app.run") ||
          isSubclass(expr1, "absl.app.run")=>
            (outerEnv ++ env).get(Id("main")) match {
              case Some(FuncSummary("main", innerAPI, innerRet)) =>
                if (api ⊔ innerAPI == Top) throw APIException
                api = api ⊔ innerAPI
              case _ => super.walk(expr)
            }

        // Normal Call expression
        case Call(EName(x), _, _) =>
          (outerEnv ++ env).get(x) match {
            case Some(FuncSummary(x.name, innerAPI, innerRet)) =>
              if (api ⊔ innerAPI == Top) throw APIException
              api = api ⊔ innerAPI
            case _ => super.walk(expr)
          }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    (env, api, retSummary)
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

case class SummaryEnv[T <: Summary](env: Map[Id, T] = Map[Id, T]()) {
  override def toString: String = toString(0)
  def toString(depth: Int): String = {
    val indent = "  " * depth
    env.foldLeft("") {
      case (str, (_, summary)) =>
        s"$str$indent${summary.toString(depth + 1)}"
    }
  }
  def apply(key: Id): T = env(key)
  def +(p: (Id, T)): SummaryEnv[T] = SummaryEnv[T](env + p)
  def ++(rhs: SummaryEnv[T]): SummaryEnv[T] = SummaryEnv[T](env ++ rhs.env)
  def get(key: Id): Option[T] = env.get(key)
  def foldLeft[B](z: B)(op: (B, (Id, T)) => B): B = env.foldLeft(z)(op)
  def contains(key: Id): Boolean = env contains key
  def find(p: ((Id, T)) => Boolean): Option[(Id, T)] = env find p
}

abstract class Summary {
  override def toString: String = toString(1)
  def toString(depth: Int): String = this match {
    case ModuleSummary(name, env, api) if depth == 1 =>
      s"└ M-$name: $api\n${env.toString(depth)}"
    case ModuleSummary(name, env, api) =>
      s"• M-$name: $api\n${env.toString(depth)}"
    case FuncSummary(name, api, retSummary) =>
      s"• F-$name -> $retSummary: $api\n"
    case ValueSummary(name, str) => s"• V-$name: $str\n"
  }
}

case class ModuleSummary(
  name: String,
  env: SummaryEnv[Summary],
  api: APIType,
) extends Summary

case class FuncSummary(
  name: String,
  api: APIType,
  retSummary: Option[ValueSummary]
) extends Summary

case class ValueSummary(
  name: String,
  value: String
) extends Summary

trait APIType {
  def ⊔(rhs: APIType): APIType = (this, rhs) match {
    case (lhs, rhs) if lhs == rhs => lhs
    case (Bot, rhs) => rhs
    case (lhs, Bot) => lhs
    case _ => Top
  }

  def ⊑(rhs: APIType): Boolean = (this, rhs) match {
    case (lhs, rhs) if lhs == rhs => true
    case (Bot, _) => true
    case (_, Top) => true
    case _ => false
  }
}

case object Sess extends APIType

case object MonSess extends APIType

case object Est extends APIType

case object GradTape extends APIType

case object Keras extends APIType

case object Bot extends APIType

case object Top extends APIType

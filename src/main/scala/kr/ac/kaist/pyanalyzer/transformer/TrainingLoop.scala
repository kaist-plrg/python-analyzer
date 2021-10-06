package kr.ac.kaist.pyanalyzer.transformer
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.util.UnitWalker
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.Console._

object TrainingLoop {
  def apply(module: Module): ModuleSummary = {
    val (env, tl) = getBodySummary(body = module.body)
    ModuleSummary(module.name, env, tl)
  }

  private def getBodySummary(
    outerEnv: TLEnv[Summary] = TLEnv[Summary](),
    body: List[Stmt]
  ): (TLEnv[Summary], TLType) = {
    // variables for storing the side effect of UnitWalker
    var env = TLEnv[Summary]()
    var tl: TLType = Bot

    // UnitWalker for summary
    object SummaryWalker extends SummaryWalker
    trait SummaryWalker extends UnitWalker {

      // update env
      override def walk(alias: Alias): Unit = alias match {
        case Alias(lx, opt) if lx.exists(x => x.name == "tensorflow") =>
          val id = opt.getOrElse(Id("tensorflow"))
          env += id -> ValueSummary(id.name, "tensor_flow")
        case _ =>
      }

      // update summary map
      override def walk(stmt: Stmt): Unit = stmt match {
        // TODO: more general imoprt walker
        case ImportFromStmt(level, List(Id("tensorflow")), List(Alias(List(Id("keras")), None))) =>
          env += Id("keras") -> ValueSummary("keras", "tensor_flow_keras")

        case FunDef(_, x, _, _, _, body) =>
          val (innerEnv, innerTl) = getBodySummary(outerEnv ++ env, body)
          env += x -> FuncSummary(x.name, innerEnv, innerTl)

        case AsyncFunDef(_, x, _, _, _, body) =>
          val (innerEnv, innerTl) = getBodySummary(outerEnv ++ env, body)
          env += x -> FuncSummary(x.name, innerEnv, innerTl)

        case ClassDef(_, x, le, lk, body) =>
          // arg summary
          var subClassOfModel = false
          object ArgWalker extends SummaryWalker {
            override def walk(expr: Expr): Unit = expr match {
              case e
                if interp(outerEnv ++ env, e) contains "tensor_flow_keras_model" =>
                  subClassOfModel = true
              case _ => super.walk(expr)
            }
          }
          le.map(ArgWalker.walk); lk.map(ArgWalker.walk)

          // TODO: consider init relation for tl
          // body summary
          val (innerEnv, _) = getBodySummary(outerEnv ++ env, body)
          env += x ->
            ClassSummary(x.name, ArgSummary(subClassOfModel), innerEnv)

        case AssignStmt(List(EName(x)), e, _)
          if isKerasModel(outerEnv ++ env, e) =>
            env += x -> ValueSummary(x.name, "model")

        case _ => super.walk(stmt)
      }

      // set tl
      override def walk(expr: Expr): Unit = expr match {
        // DistributedGradientTape training loop identifier
        case Call(Attribute(EName(x), Id("GradientTape")), Nil, Nil)
          if (outerEnv ++ env).get(x) contains ValueSummary(x.name, "tensor_flow") =>
            // multiple training loops
            if (tl == Optimizer) throw TLException
            tl = GradTape

        // DistributedOptimizer training loop identifier
        case Call(Attribute(model, Id("fit")), _, _)
          if isKerasModel(outerEnv ++ env, model) =>
            // multiple training loops
            if (tl == GradTape) throw TLException
            tl = Optimizer

        // Normal Call expression
        case Call(EName(x), _, _) =>
          (outerEnv ++ env).get(x) match {
            case Some(FuncSummary(x.name, innerEnv, innerTl)) if innerTl != Bot =>
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

  // TODO: using interp
  // identify tensorflow.keras.models
  private def isKerasModel(
    env: TLEnv[Summary],
    model: Expr
  ): Boolean = model match {
    // Sequential API
    case Call(Attribute(Attribute(Attribute(
      EName(tf), Id("keras")), Id("models")), Id("Sequential")), _, _)
        if env.get(tf) contains ValueSummary(tf.name, "tensor_flow") => true

    // Funtional API
    case Call(Attribute(Attribute(EName(tf), Id("keras")), Id("Model")), _, _)
      if env.get(tf) contains ValueSummary(tf.name, "tensor_flow") => true
    case Call(EName(model), _, _)
      if env.get(model) contains ValueSummary(model.name, "tensor_flow_keras_model") => true

    // Subclassing API
    case Call(EName(subclass), _, _) => env.get(subclass) match {
      case Some(ClassSummary(_, argSummary, _)) => argSummary.parent
      case _ => false
    }

    // already defined in environment
    case EName(x) if env.get(x) contains ValueSummary(x.name, "model") => true
    case _ => false
  }

  // TODO: update import
  private def interp(
    env: TLEnv[Summary],
    e: Expr
  ): Option[String] = e match {
    case EName(x) => env.get(x) match {
      case Some(ValueSummary(_, str)) => Some(str)
      case _ => None
    }
    case Attribute(e, x) => interp(env, e) match {
      case Some("tensor_flow") if x == Id("keras") => Some("tensor_flow_keras")
      case Some("tensor_flow_keras") if x == Id("Model") =>
        Some("tensor_flow_keras_model")
      case _ => None
    }
    case _ => None
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
    case ModelSummary(name, env, tl) =>
       env.foldLeft(s"$CYAN<$name> @ $tl$RESET\n") {
         case (acc, (name, summary)) => s"$acc${summary.toString(depth + 1)}"
       }
    case ModuleSummary(name, env, tl) if depth == 2 =>
      s"└ M-$name: $tl\n${env.toString(depth)}"
    case ModuleSummary(name, env, tl) =>
      s"• M-$name: $tl\n${env.toString(depth)}"
    case FuncSummary(name, env, tl) =>
      s"• F-$name: $tl\n${env.toString(depth)}"
    case ClassSummary(name, argSummary, env) =>
      s"• C-$name($argSummary)\n${env.toString(depth)}"
    case ValueSummary(name, str) => s"• V-$name: $str\n"
  }
}

case class ModelSummary(
  name: String,
  env: TLEnv[ModuleSummary],
  tl: TLType
) extends Summary {
  def getModuleSummary(name: String): ModuleSummary = env(Id(name))
}

case class ModuleSummary(
  name: String,
  env: TLEnv[Summary],
  tl: TLType,
) extends Summary

case class FuncSummary(
  name: String,
  env: TLEnv[Summary],
  tl: TLType,
) extends Summary

case class ClassSummary(
  name: String,
  argSummary: ArgSummary,
  env: TLEnv[Summary],
) extends Summary

case class ValueSummary(
  name: String,
  value: String
) extends Summary

// TODO: more general arg summary
case class ArgSummary(parent: Boolean) {
  override def toString = if (parent) "Model" else ""
}

trait TLType

case object GradTape extends TLType

case object Optimizer extends TLType

case object Bot extends TLType

trait SumType

case object TLModule extends SumType

case object TLFunc extends SumType

case object TLClass extends SumType

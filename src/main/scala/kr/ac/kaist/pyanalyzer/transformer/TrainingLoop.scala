package kr.ac.kaist.pyanalyzer.transformer
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.UnitWalker
import kr.ac.kaist.pyanalyzer.util.Errors._
import scala.Console._


object TrainingLoop {
  def apply(name: String, modules: Iterable[Module]): ModelSummary = {
    val cache = modules.foldLeft(TLEnv[ModuleSummary]())((cache, module) =>
      if (cache contains module.name) cache
      else getModuleSummary(cache, module)
    )
    // TODO: handle TL Error
    val mainScriptSummary = cache find {
      case (name, summary) => summary.tl != Bot
    }
    ModelSummary(name, cache, Bot)
  }

  private def updateCache(
    sumMap: TLEnv[Summary],
    cache: TLEnv[ModuleSummary]
  ): TLEnv[ModuleSummary] = sumMap.foldLeft(cache) {
    case (newCache, (name, summary: ModuleSummary)) => newCache + (name -> summary)
    case (newCache, _) => newCache
  }

  private def getModuleSummary(
    cache: TLEnv[ModuleSummary],
    m: Module
  ): TLEnv[ModuleSummary] = {
    val (sumMap, tl) = getBodySummary(cache, body = m.body)
    updateCache(sumMap, cache) + (m.name -> ModuleSummary(m.name, sumMap, tl))
  }

  private def getBodySummary(
    cache: TLEnv[ModuleSummary],
    outerEnv: Map[Id, String] = Map(),
    outerSumMap: TLEnv[Summary] = TLEnv[Summary](),
    body: List[Stmt]
  ): (TLEnv[Summary], TLType) = {
    // variables for storing the side effect of UnitWalker
    var env = outerEnv
    var sumMap = TLEnv[Summary]()
    var tl: TLType = Bot

    // UnitWalker for summary
    object SummaryWalker extends SummaryWalker
    trait SummaryWalker extends UnitWalker {

      // update env
      override def walk(alias: Alias): Unit = alias match {
        case Alias(lx, opt) if lx.exists(x => x.name == "tensorflow") =>
          env += opt.getOrElse(Id("tensorflow")) -> "tensor_flow"
        case _ =>
      }

      // update summary map
      override def walk(stmt: Stmt): Unit = stmt match {
        // TODO: more general imoprt walker
        case ImportFromStmt(level, List(Id("tensorflow")), List(Alias(List(Id("keras")), None))) =>
          env += Id("keras") -> "tensor_flow_keras"

        case FunDef(_, x, _, _, _, body) =>
          val (innerSumMap, innerTl) = getBodySummary(cache, env, outerSumMap ++ sumMap, body)
          sumMap += x.name -> FuncSummary(x.name, innerSumMap, innerTl)

        case AsyncFunDef(_, x, _, _, _, body) =>
          val (innerSumMap, innerTl) = getBodySummary(cache, env, outerSumMap ++ sumMap, body)
          sumMap += x.name -> FuncSummary(x.name, innerSumMap, innerTl)

        case ClassDef(_, x, le, lk, body) =>
          val newSumMap = outerSumMap ++ sumMap

          // arg summary
          var subClassOfModel = false
          object ArgWalker extends SummaryWalker {
            override def walk(expr: Expr): Unit = expr match {
              case e
                if interp(env, newSumMap, e) contains "tensor_flow_keras_model" =>
                  subClassOfModel = true
              case _ => super.walk(expr)
            }
          }
          le.map(ArgWalker.walk); lk.map(ArgWalker.walk)

          // TODO: consider init relation for tl
          // body summary
          val (innerSumMap, _) = getBodySummary(cache, env, newSumMap, body)
          sumMap += x.name ->
            ClassSummary(x.name, ArgSummary(subClassOfModel), innerSumMap)

        case AssignStmt(List(EName(x)), e, _)
          if isKerasModel(env, outerSumMap ++ sumMap, e) =>
            env += x -> "model"

        case _ => super.walk(stmt)
      }

      // set tl
      override def walk(expr: Expr): Unit = expr match {
        // DistributedGradientTape training loop identifier
        case Call(Attribute(EName(x), Id("GradientTape")), Nil, Nil)
          if env.get(x) contains "tensor_flow" =>
            // multiple training loops
            if (tl == Optimizer) throw TLException
            tl = GradTape

        // DistributedOptimizer training loop identifier
        case Call(Attribute(model, Id("fit")), _, _)
          if isKerasModel(env, outerSumMap ++ sumMap, model) =>
            // multiple training loops
            if (tl == GradTape) throw TLException
            tl = Optimizer

        // Normal Call expression
        case Call(EName(Id(name)), _, _) =>
          (outerSumMap ++ sumMap).get(name) match {
            case Some(FuncSummary(name, innerSumMap, innerTl)) if innerTl != Bot =>
              tl = innerTl
            case Some(ModuleSummary(name, innerSumMap, innerTl)) if innerTl != Bot =>
              tl = innerTl
            // TODO: Add ClassSummary case
            case _ => super.walk(expr)
          }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    (sumMap, tl)
  }

  // TODO: using interp
  // identify tensorflow.keras.models
  private def isKerasModel(
    env: Map[Id, String],
    sumMap: TLEnv[Summary],
    model: Expr
  ): Boolean = model match {
    // Sequential API
    case Call(Attribute(Attribute(Attribute(
      EName(tf), Id("keras")), Id("models")), Id("Sequential")), _, _)
        if env.get(tf) contains "tensor_flow" => true

    // Funtional API
    case Call(Attribute(Attribute(EName(tf), Id("keras")), Id("Model")), _, _)
      if env.get(tf) contains "tensor_flow" => true
    case Call(EName(model), _, _)
      if env.get(model) contains "tensor_flow_keras_model" => true

    // Subclassing API
    case Call(EName(subclass), _, _) => sumMap.get(subclass.name) match {
      case Some(ClassSummary(_, argSummary, _)) => argSummary.parent
      case _ => false
    }

    // already defined in environment
    case EName(x) if env.get(x) contains "model" => true
    case _ => false
  }

  // TODO: update import
  private def interp(
    env: Map[Id, String],
    sumMap: TLEnv[Summary],
    e: Expr
  ): Option[String] = e match {
    case EName(x) => env.get(x)
    case Attribute(e, x) => interp(env, sumMap, e) match {
      case Some("tensor_flow") if x == Id("keras") => Some("tensor_flow_keras")
      case Some("tensor_flow_keras") if x == Id("Model") =>
        Some("tensor_flow_keras_model")
      case _ => None
    }
    case _ => None
  }
}

case class TLEnv[T <: Summary](sumMap: Map[String, T] = Map[String, T]()) {
  override def toString: String = toString(0)
  def toString(depth: Int): String = {
    val indent = "  " * depth
    sumMap.foldLeft("") {
      case (str, (name, summary)) =>
        s"$str$indent${summary.toString(depth + 1)}"
    }
  }
  def apply(key: String): T = sumMap(key)
  def +(p: (String, T)): TLEnv[T] = TLEnv[T](sumMap + p)
  def ++(rhs: TLEnv[T]): TLEnv[T] = TLEnv[T](sumMap ++ rhs.sumMap)
  def get(key: String): Option[T] = sumMap.get(key)
  def foldLeft[B](z: B)(op: (B, (String, T)) => B): B = sumMap.foldLeft(z)(op)
  def contains(key: String): Boolean = sumMap contains key
  def find(p: ((String, T)) => Boolean): Option[(String, T)] = sumMap find p
}

abstract class Summary {
  override def toString: String = toString(1)
  def toString(depth: Int): String = this match {
    case ModelSummary(name, sumMap, tl) =>
       sumMap.foldLeft(s"$CYAN<$name> @ $tl$RESET\n") {
         case (acc, (name, summary)) => s"$acc${summary.toString(depth + 1)}"
       }
    case ModuleSummary(name, sumMap, tl) if depth == 2 =>
      s"└ M-$name: $tl\n${sumMap.toString(depth)}"
    case ModuleSummary(name, sumMap, tl) =>
      s"• M-$name: $tl\n${sumMap.toString(depth)}"
    case FuncSummary(name, sumMap, tl) =>
      s"• F-$name: $tl\n${sumMap.toString(depth)}"
    case ClassSummary(name, argSummary, sumMap) =>
      s"• C-$name($argSummary)\n${sumMap.toString(depth)}"
  }
}

case class ModelSummary(
  name: String,
  sumMap: TLEnv[ModuleSummary],
  tl: TLType
) extends Summary

case class ModuleSummary(
  name: String,
  sumMap: TLEnv[Summary],
  tl: TLType,
) extends Summary

case class FuncSummary(
  name: String,
  sumMap: TLEnv[Summary],
  tl: TLType,
) extends Summary

case class ClassSummary(
  name: String,
  argSummary: ArgSummary,
  sumMap: TLEnv[Summary],
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

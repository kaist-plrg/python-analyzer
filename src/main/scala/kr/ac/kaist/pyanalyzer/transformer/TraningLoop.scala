package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.UnitWalker
import kr.ac.kaist.pyanalyzer.util.Errors._

abstract class Summary {
  val name: String
  val sumMap: Map[String, Summary]
  val tl: TLType
  override def toString: String = toString(0)
  def toString(outerDepth: Int): String = {
    val depth = outerDepth + 1
    val indent = "  " * depth
    sumMap.foldLeft(s"● $name: $tl\n") {
      case (str, (name, summary)) =>
        s"$str$indent${summary.toString(depth)}"
    }
  }
}

case class ModuleSummary(
  name: String,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary

case class FuncSummary(
  name: String,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary

case class ClassSummary(
  name: String,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary


object TrainingLoop {
  def apply(m: Module) = {
    val (sumMap, tl) = getSummary(body = m.body)
    ModuleSummary(m.name, sumMap, tl)
  }

  private def getSummary(
    outerEnv: Map[String, Id] = Map(),
    outerSumMap: Map[String, Summary] = Map(),
    body: List[Stmt]
  ): (Map[String, Summary], TLType) = {
    // variables for storing the side effect of UnitWalker
    var env = outerEnv
    var sumMap = Map[String, Summary]()
    var tl: TLType = Bot

    // UnitWalker for summary
    object SummaryWalker extends UnitWalker {

      // update env
      override def walk(alias: Alias): Unit = alias match {
        case Alias(lx, opt) if lx.exists(x => x.name == "tensorflow") =>
          env += "tensor_flow" -> opt.getOrElse(Id("tensorflow"))
        case _ =>
      }

      // update summary map
      override def walk(stmt: Stmt): Unit = stmt match {
        case FunDef(_, x, _, _, _, body) =>
          val (innerSumMap, innerTl) = getSummary(env, outerSumMap ++ sumMap, body)
          sumMap += x.name -> FuncSummary(x.name, innerSumMap, innerTl)

        // TODO: ClassDef
        case AssignStmt(List(EName(x)), e, _)
          if isKerasModel(env, outerSumMap ++ sumMap, e) =>
            env += "tensor_flow_keras_model" -> x

        case _ => super.walk(stmt)
      }

      // set tl
      override def walk(expr: Expr): Unit = expr match {
        // DistributedGradientTape training loop identifier
        case Call(Attribute(EName(x), Id("GradientTape")), Nil, Nil)
          if env.get("tensor_flow") contains x =>
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
            case sopt: Some[Summary] if sopt.get.tl != Bot => tl = sopt.get.tl
            case _ => super.walk(expr)
          }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    (sumMap, tl)
  }

  // TODO: interp
  // TODO: add more cases
  // identify tensorflow.keras.models
  private def isKerasModel(
    env: Map[String, Id],
    sumMap: Map[String, Summary],
    model: Expr
  ): Boolean = model match {
    // Sequential API
    // tf.keras.models.Sequential(_)
    case Call(Attribute(Attribute(Attribute(
      EName(tf), Id("keras")), Id("models")), Id("Sequential")), _, _)
        if env.get("tensor_flow") contains tf => true

    // Funtional API
    // tf.keras.Model(_)
    case Call(Attribute(Attribute(EName(tf), Id("keras")), Id("Model")), _, _)
      if env.get("tensor_flow") contains tf => true
    // Model(_)
    case Call(EName(model), _, _)
      if env.get("tensor_flow_keras_model_module") contains model => true

    // in environment
    case EName(x) if env.get("tensor_flow_keras_model") contains x => true
    case _ => false
  }
}

trait TLType

case object GradTape extends TLType

case object Optimizer extends TLType

case object Bot extends TLType

trait SumType

case object TLModule extends SumType

case object TLFunc extends SumType

case object TLClass extends SumType

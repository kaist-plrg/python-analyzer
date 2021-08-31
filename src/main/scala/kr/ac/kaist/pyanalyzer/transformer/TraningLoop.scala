package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.UnitWalker
import kr.ac.kaist.pyanalyzer.util.Errors._

abstract class Summary {
  val sumMap: Map[String, Summary]
  val info: String
  val tl: TLType
  override def toString: String = toString(0)
  def toString(outerDepth: Int): String = {
    val depth = outerDepth + 1
    val indent = "  " * depth
    sumMap.foldLeft(info) {
      case (str, (name, summary)) =>
        s"$str$indent${summary.toString(depth)}"
    }
  }
}

case class ModuleSummary(
  name: String,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary {
  val info = s"● $name: $tl\n"
}

case class FuncSummary(
  name: String,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary {
  val info = s"● $name: $tl\n"
}

case class ClassSummary(
  name: String,
  argSummary: ArgSummary,
  sumMap: Map[String, Summary],
  tl: TLType,
) extends Summary {
  val info = s"● $name($argSummary): $tl\n"
}

// TODO: more general arg summary
case class ArgSummary(parent: Boolean) {
  override def toString = if (parent) "Model" else ""
}


object TrainingLoop {
  def apply(m: Module) = {
    val (sumMap, tl) = getSummary(body = m.body)
    ModuleSummary(m.name, sumMap, tl)
  }

  private def getSummary(
    outerEnv: Map[Id, String] = Map(),
    outerSumMap: Map[String, Summary] = Map(),
    body: List[Stmt]
  ): (Map[String, Summary], TLType) = {
    // variables for storing the side effect of UnitWalker
    var env = outerEnv
    var sumMap = Map[String, Summary]()
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
          val (innerSumMap, innerTl) = getSummary(env, outerSumMap ++ sumMap, body)
          sumMap += x.name -> FuncSummary(x.name, innerSumMap, innerTl)

        case AsyncFunDef(_, x, _, _, _, body) =>
          val (innerSumMap, innerTl) = getSummary(env, outerSumMap ++ sumMap, body)
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
          val (innerSumMap, innerTl) = getSummary(env, newSumMap, body)
          sumMap += x.name ->
            ClassSummary(x.name, ArgSummary(subClassOfModel), innerSumMap, innerTl)

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
            case sopt: Some[Summary] if sopt.get.tl != Bot => tl = sopt.get.tl
            case _ => super.walk(expr)
          }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    (sumMap, tl)
  }

  def getArgInfo(
    env: Map[Id, String],
    sumMap: Map[String, Summary],
    le: List[Expr],
    lk: List[Kwarg]
  ): ArgSummary = ???

  // TODO: using interp
  // identify tensorflow.keras.models
  private def isKerasModel(
    env: Map[Id, String],
    sumMap: Map[String, Summary],
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
      case Some(ClassSummary(_, argSummary, _, _)) => argSummary.parent
      case _ => false
    }

    // already defined in environment
    case EName(x) if env.get(x) contains "model" => true
    case _ => false
  }

  // TODO: update import
  private def interp(
    env: Map[Id, String],
    sumMap: Map[String, Summary],
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

trait TLType

case object GradTape extends TLType

case object Optimizer extends TLType

case object Bot extends TLType

trait SumType

case object TLModule extends SumType

case object TLFunc extends SumType

case object TLClass extends SumType

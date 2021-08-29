package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.UnitWalker

case class Summary(sumMap: Map[String, Summary], tl: TLType) {
  override def toString: String = toString(0)
  private def toString(depth: Int): String = {
    val indent = "  " * depth
    sumMap.foldLeft(s"$tl\n") {
      case (str, (name, summary)) =>
        s"$str$indent● $name: ${summary.toString(depth + 1)}"
    }
  }
}

object TrainingLoop {
  def apply(m: Module) = getSummary(Map(), Map(), m.body)

  private def getSummary(
    env: Map[String, Id],
    outerSumMap: Map[String, Summary],
    body: List[Stmt]
  ): Summary = {
    // variables for storing the side effect of UnitWalker
    var newEnv = env
    var sumMap = Map[String, Summary]()
    var tl: TLType = Bot

    // UnitWalker for summary
    object SummaryWalker extends UnitWalker {
      override def walk(alias: Alias): Unit = alias match {
        case Alias(lx, opt) if lx.exists(x => x.name == "tensorflow") =>
          newEnv += "tensor_flow" -> opt.getOrElse(Id("tensorflow"))
        case _ =>
      }
      override def walk(stmt: Stmt): Unit = stmt match {
        case FunDef(_, x, _, _, _, body) =>
          sumMap += x.name -> getSummary(newEnv, outerSumMap ++ sumMap, body)
        // TODO: ClassDef
        case _ => super.walk(stmt)
      }
      override def walk(expr: Expr): Unit = expr match {
        case Call(Attribute(EName(x), Id("GradientTape")), Nil, Nil)
          if env.get("tensor_flow") contains x => tl = GradTape
        case Call(EName(Id(name)), _, _) => (outerSumMap ++ sumMap).get(name) match {
          case Some(Summary(_, newTl)) if newTl != Bot => tl = newTl
          case _ => super.walk(expr)
        }
        case _ => super.walk(expr)
      }
    }

    body.map(SummaryWalker.walk)
    Summary(sumMap, tl)
  }
}

trait TLType

case object GradTape extends TLType

case object Optimizer extends TLType

case object Bot extends TLType

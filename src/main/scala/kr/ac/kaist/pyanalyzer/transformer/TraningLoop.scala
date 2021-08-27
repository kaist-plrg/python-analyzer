package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.UnitWalker

object TrainingLoop extends TrainingLoop

class TrainingLoop {
  def apply(m: Module): List[String]  = ???

  def getSummary(body: List[Stmt])(implicit env: Map[String, Id]): Summary = {
    var names = List[String]()
    var tl: Train = NoTL
    object BodyWalker extends UnitWalker {
      override def walk(e: Expr): Unit = e match {
        case Call(Attribute(EName(x), Id("GradientTape")), Nil, Nil) =>
          tl = GradTape
        case Call(EName(x), _, _) => names = x.name :: names
        case _ => super.walk(e)
      }
    }
    body.map(BodyWalker.walk)
    Summary(names, tl)
  }

  def getSummary(m: Module): Map[String, Summary] = {
    val walker = new SummaryWalker
    walker(m)
    val summary = walker.summary
    val tl = tlkind(summary)
    val res = summary + ("$entry" -> summary("$entry").copy(tl = tl))
    res.foreach(println)
    res
  }

  def tlkind(summary: Map[String, Summary]) = {
    var tlnn: Train = NoTL
    summary map {
      case (name, Summary(_, GradTape))
        if summary("$entry").names contains name => tlnn = GradTape
      case _ =>
    }
    tlnn
  }
}

case class Summary(names: List[String], tl: Train)

trait Train

case object GradTape extends Train

case object NoTL extends Train

class SummaryWalker extends UnitWalker {
  import TrainingLoop._
  var summary = Map[String, Summary]()
  implicit var env = Map[String, Id]()
  override def apply(m: Module): Unit = {
    summary += "$entry" -> getSummary(m.body)
    super.apply(m)
  }
  override def walk(alias: Alias): Unit = alias match {
    case Alias(lx, opt) if lx.contains((x: Id) => x.name == "tensorflow") =>
      env += "tensor_flow" -> opt.getOrElse(Id("tensorflow"))
    case _ =>
  }
  override def walk(stmt: Stmt): Unit = stmt match {
    case FunDef(_, x, _, _, _, body) =>
      //super.walk(stmt)
      summary += x.name -> getSummary(body)
    case AsyncFunDef(_, x, _, _, _, body) =>
      //super.walk(stmt)
      summary += x.name -> getSummary(body)
    // TODO: consider subclassing
    // case c: ClassDef => ???
    case _ => super.walk(stmt)
  }
}

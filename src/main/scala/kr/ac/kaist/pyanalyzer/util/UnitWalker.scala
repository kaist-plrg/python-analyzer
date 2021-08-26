package kr.ac.kaist.pyanalyzer.util

import kr.ac.kaist.pyanalyzer.parser.ast._

trait UnitWalker {
  def apply(m: Module): Unit = m.body.map(walk)
  def walk(str: String): Unit = ()
  def walk(id: Id): Unit = walk(id.name)
  def walk(di: DictItem): Unit = di match {
    case KVPair(e1, e2) => walk(e1); walk(e2)
    case DoubleStarred(e) => walk(e)
  }
  def walk(e: Expr): Unit = e match {
    case BoolExpr(_, e1, e2) => walk(e1); walk(e2)
    case BoolGroupExpr(_, le) => le.map(walk)
    case NamedExpr(e1, e2) => walk(e1); walk(e2)
    case BinaryExpr(_, e1, e2) => walk(e1); walk(e2)
    case UnaryExpr(_, e) => walk(e)
    case LambdaExpr(args, e) => walk(args); walk(e)
    case IfExpr(e1, e2, e3) => walk(e1); walk(e2); walk(e3)
    case DictExpr(ld) => ld.map(walk)
    case SetExpr(le) => le.map(walk)
    case ListExpr(le) => le.map(walk)
    case TupleExpr(le) => le.map(walk)
    case ListComp(e, lc) => walk(e); lc.map(walk)
    case SetComp(e, lc) => walk(e); lc.map(walk)
    case DictComp(pe, lc) =>
      val pWalk: ((Expr, Expr)) => Unit = {
        case (e1, e2) => walk(e1); walk(e2)
      }
      pWalk(pe); lc.map(walk)
    case GenComp(e, lc) => walk(e); lc.map(walk)
    case AwaitExpr(e) => walk(e)
    case YieldExpr(oe) => oe.map(walk)
    case YieldFromExpr(e) => walk(e)
    case CompExpr(e, lp) =>
      val pWalk: ((CompOp, Expr)) => Unit  = {
        case (_, e) => walk(e)
      }
      walk(e); lp.map(pWalk)
    case Call(e, le, lk) => walk(e); le.map(walk); lk.map(walk)
    case FormattedValue(e, _, oe) => walk(e); oe.map(walk)
    case JoinedStr(le) => le.map(walk)
    case EConst(_) =>
    case Attribute(e, x) => walk(e); walk(x)
    case Subscript(e1, e2) => walk(e1); walk(e2)
    case Starred(e) => walk(e)
    case EName(x) => walk(x)
    case Slice(oe1, oe2, oe3) => oe1.map(walk); oe2.map(walk); oe3.map(walk)
    case GroupExpr(e) => ???
  }
  def walk(stmt: Stmt): Unit = stmt match {
    case FunDef(decos, name, args, oe, ostr, body) =>
      decos.map(walk); walk(name); walk(args); oe.map(walk);
      ostr.map(walk); body.map(walk)
    case AsyncFunDef(decos, name, args, oe, ostr, body) =>
      decos.map(walk); walk(name); walk(args); oe.map(walk);
      ostr.map(walk); body.map(walk)
    case ClassDef(decos, name, le, kwds, body) =>
      decos.map(walk); walk(name); le.map(walk); kwds.map(walk);
      body.map(walk)
    case ReturnStmt(oe) => oe.map(walk)
    case DelStmt(le) => le.map(walk)
    case AssignStmt(le, e, ostr) => le.map(walk); walk(e); ostr.map(walk)
    case AugAssign(e1, op, e2) => walk(e1); walk(e2)
    case AnnAssign(e1, e2, oe) => walk(e1); walk(e2); oe.map(walk)
    case ForStmt(ostr,e1, e2, ls1, ls2) =>
      ostr.map(walk); walk(e1); walk(e2); ls1.map(walk); ls2.map(walk)
    case AsyncForStmt(ostr,e1, e2, ls1, ls2) =>
      ostr.map(walk); walk(e1); walk(e2); ls1.map(walk); ls2.map(walk)
    case WhileStmt(e, ls1, ls2) => walk(e); ls1.map(walk); ls2.map(walk)
    case IfStmt(e, ls1, ls2) => walk(e); ls1.map(walk); ls2.map(walk)
    case WithStmt(ostr, lw, ls) => ostr.map(walk); lw.map(walk); ls.map(walk)
    case AsyncWithStmt(ostr, lw, ls) =>
      ostr.map(walk); lw.map(walk); ls.map(walk)
    case MatchStmt(e, lmc) => walk(e); lmc.map(walk)
    case RaiseStmt(oe1, oe2) => oe1.map(walk); oe2.map(walk)
    case TryStmt(ls1, lexc, ls2, ls3) =>
      ls1.map(walk); lexc.map(walk); ls2.map(walk); ls3.map(walk)
    case AssertStmt(e, oe) => walk(e); oe.map(walk)
    case ImportStmt(la) => la.map(walk)
    case ImportFromStmt(_, lx, la) => lx.map(walk); la.map(walk)
    case GlobalStmt(lx) => lx.map(walk)
    case NonlocalStmt(lx) => lx.map(walk)
    case ExprStmt(e) => walk(e)
    case OnelineStmt(ls) => ???
    case Comment(c) => walk(c)
    case _ =>
  }
  def walk(argument: Argument): Unit = argument match {
    case Args(lp1, lp2, oarg1, lp3, oarg2) =>
      val pWalk: ((Arg, Option[Expr])) => Unit = {
        case (arg, oe) => walk(arg); oe.map(walk)
      }
      lp1.map(pWalk); lp1.map(pWalk); oarg1.map(walk); lp1.map(pWalk);
      oarg2.map(walk)
    case Arg(x, oe, ostr) => walk(x); oe.map(walk); ostr.map(walk)
    case NormalKwarg(x, e) => walk(x); walk(e)
    case DoubleStarredKwarg(e) => walk(e)
    case Keyword(ox, e) => ox.map(walk); walk(e)
  }
  def walk(comp: Comprehension): Unit = comp match {
    case Compre(e1, e2, le) => walk(e1); walk(e2); le.map(walk)
    case AsyncCompre(e1, e2, le) => walk(e1); walk(e2); le.map(walk)
  }
  def walk(exc: ExcHandler): Unit = exc match {
    case ExcHandler(oe, ox, ls) => oe.map(walk); ox.map(walk); ls.map(walk)
  }
  def walk(alias: Alias): Unit = alias match {
    case Alias(lx, ox) => lx.map(walk); ox.map(walk)
  }
  def walk(wi: WithItem): Unit = wi match {
    case WithItem(e, oe) => walk(e); oe.map(walk)
  }
  def walk(mc: MatchCase): Unit = mc match {
    case MatchCase(pat, oe, ls) => walk(pat); oe.map(walk); ls.map(walk)
  }
  def walk(pat: Pattern): Unit = pat match {
    case MatchValue(e) => walk(e)
    case MatchSingleton(_) =>
    case MatchSeq(lpat) => lpat.map(walk)
    case MatchStar(ox) => ox.map(walk)
    case MatchMapping(lp, ox) =>
      val pWalk: ((Expr, Pattern)) => Unit = {
        case (e, pat) => walk(e); walk(pat)
      }
      lp.map(pWalk); ox.map(walk)
    case MatchClass(e, lpat, lp) =>
      val pWalk: ((Id, Pattern)) => Unit = {
        case (x, pat) => walk(x); walk(pat)
      }
      walk(e); lpat.map(walk); lp.map(pWalk)
    case MatchAs(opat, x) => opat.map(walk); walk(x)
    case MatchOr(lpat) => lpat.map(walk)
    case MatchWildcard =>
    case MatchGroup(pat) => walk(pat)
  }
}

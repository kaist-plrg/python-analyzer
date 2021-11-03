package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser.ast._

trait TransformerWalker {
  /////////////////////////////////////////
  // transformer for statements
  /////////////////////////////////////////
  def transform(stmts: List[Stmt])(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) =
    stmts.foldLeft((List[Stmt](), env, List[Warning]())) {
      case ((stmtList, e, lw), stmt) =>
        val (newStmtList, newEnv, newlw) = transform(stmt)(e)
        (stmtList ++ newStmtList, newEnv, lw ++ newlw)
    }

  def transform(stmt: Stmt)(
    implicit env: Env
  ): (List[Stmt], Env, List[Warning]) = stmt match {
    // function def
    case FunDef(decos, name, args, retTy, tyExpr, body) =>
      val (newBody, _, lw) = transform(body)
      (FunDef(decos, name, args, retTy, tyExpr, newBody), env, lw)
    case AsyncFunDef(decos, name, args, retTy, tyExpr, body) =>
      val (newBody, _, lw) = transform(body)
      (AsyncFunDef(decos, name, args, retTy, tyExpr, newBody), env, lw)

    // class def
    case ClassDef(decos, name, exprs, kwds, body) =>
      val (newBody, _, lw) = transform(body)
      (ClassDef(decos, name, exprs, kwds, newBody), env, lw)

    // return, del
    case ReturnStmt(eopt) =>
      (ReturnStmt(eopt.map(expr => transform(expr))), env)
    case DelStmt(tl) => (DelStmt(tl), env)
    // AssignStmt that targets is non-singular or non-id
    case AssignStmt(targets, e, ty) => (AssignStmt(targets, transform(e), ty), env)
    // AugAssign case
    case AugAssign(lhs, bop, rhs) => (AugAssign(lhs, bop, transform(rhs)), env)
    // AnnAssign case: 
    case AnnAssign(e1, e2, e3) => (AnnAssign(e1, e2, e3.map(transform)), env)

    /////////////////////////////////////////////////////////////////
    // for statement
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      val (newDoStmt, _, dolw) = transform(doStmt)
      val (newElseStmt, _, elselw) = transform(elseStmt)
      val newStmt =
        ForStmt(ty, forExpr, transform(inExpr), newDoStmt, newElseStmt)
      (newStmt, env, dolw ++ elselw)
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      val (newDoStmt, _, dolw) = transform(doStmt)
      val (newElseStmt, _, elselw) = transform(elseStmt)
      val newStmt =
        AsyncForStmt(ty, forExpr, transform(inExpr), newDoStmt, newElseStmt)
      (newStmt, env, dolw ++ elselw)
    // while statement
    case WhileStmt(wExpr, doStmt, elseStmt) =>
      val (newDoStmt, _, dolw) = transform(doStmt)
      val (newElseStmt, _, elselw) = transform(elseStmt)
      val newStmt =
        WhileStmt(transform(wExpr), newDoStmt, newElseStmt)
      (newStmt, env, dolw ++ elselw) 
    // if statement
    case IfStmt(cond, thenStmt, elseStmt) =>
      val (newThenStmt, _, thenlw) = transform(thenStmt)
      val (newElseStmt, _, elselw) = transform(elseStmt)
      val newStmt =
        IfStmt(transform(cond), newThenStmt, newElseStmt)
      (newStmt, env, thenlw ++ elselw)

    /////////////////////////////////////////////////////////////////
    // with statement
    /////////////////////////////////////////////////////////////////
    case WithStmt(ty, items, doStmt) =>
      val (newItems, interEnv) = transformWithList(items)
      val (newStmts, newEnv, newWarning) = transform(doStmt)(interEnv)
      (WithStmt(ty, newItems, newStmts), newEnv, newWarning)
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, interEnv) = transformWithList(items)
      val (newStmts, newEnv, newWarning) = transform(doStmt)(interEnv)
      (WithStmt(ty, newItems, newStmts), newEnv, newWarning)

    /////////////////////////////////////////////////////////////////
    // match statement
    /////////////////////////////////////////////////////////////////
    case MatchStmt(expr, cases) =>
      val (newCases, lw) =
        cases.foldRight((List[MatchCase](), List[Warning]())) {
          case (c, (lc, lw)) =>
            val (newCase, newWarning) = transform(c)
            (newCase :: lc, newWarning ++ lw)
        }
      (MatchStmt(transform(expr), newCases), env, lw)

    // exception-related statements
    case RaiseStmt(expr, from) =>
      (RaiseStmt(expr, from), env)
    case TryStmt(tryStmt, handlers, elseStmt, finallyStmt) =>
      val (newTryStmt, _, tryWarning) = transform(tryStmt)
      val (newElseStmt, _, elseWarning) = transform(elseStmt)
      val (newHandlers, handlerWarning) =
        handlers.foldRight(List[ExcHandler](), List[Warning]()) {
          case (handler, (lh, lw)) =>
            val (newHandler, newlw) = transform(handler)
            (newHandler :: lh, newlw ++ lw)
        }
      val (newFinallyStmt, _, finallyWarning) = transform(finallyStmt)
      val newStmt =
        TryStmt(
          newTryStmt,
          newHandlers,
          newElseStmt,
          newFinallyStmt)
      (newStmt, env,
        tryWarning ++ handlerWarning ++ elseWarning ++ finallyWarning)
    case AssertStmt(expr, toRaise) =>
      (AssertStmt(transform(expr), toRaise), env)

    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportStmt(alias) => (ImportStmt(alias), transform(alias))

    /////////////////////////////////////////////////////////////////
    // other scope-related
    case ImportFromStmt(lv, fromId, al) => (ImportFromStmt(lv, fromId, al), transform(al))
    case GlobalStmt(il) => (GlobalStmt(il), env)
    case NonlocalStmt(il) => (NonlocalStmt(il), env)


    /////////////////////////////////////////////////////////////////
    // expr stmt
    case ExprStmt(e) => (ExprStmt(transform(e)), env)
    case PassStmt => (PassStmt, env)
    case BreakStmt => (BreakStmt, env)
    case ContinueStmt => (ContinueStmt, env)
    // TODO: check transform from simple to compound exists
    case OnelineStmt(ls) =>
      val (newLs, newEnv, lw) = transform(ls)
      (OnelineStmt(newLs), newEnv, lw)
    case Comment(c) => (Comment(c), env)
  }

  /////////////////////////////////////////
  // transformer for Expression
  /////////////////////////////////////////
  def transform(expr: Expr)(implicit env: Env): Expr = expr match {
    case BoolExpr(op, lhs, rhs) => 
      BoolExpr(op, transform(lhs), transform(rhs))
    case NamedExpr(lhs, rhs) => 
      NamedExpr(lhs, transform(rhs))
    case BinaryExpr(op, lhs, rhs) => 
      BinaryExpr(op, transform(lhs), transform(rhs))
    case UnaryExpr(op, e) => 
      UnaryExpr(op, transform(e))
    case LambdaExpr(args, e) => 
      LambdaExpr(args, transform(e))
    case IfExpr(e, cond, ee) => 
      IfExpr(transform(e),transform(cond),transform(ee))
    case DictExpr(map) => DictExpr(map.map(transform))
    case SetExpr(set) => SetExpr(set.map(transform))
    case ListExpr(list) => ListExpr(list.map(transform))
    case TupleExpr(tup) => TupleExpr(tup.map(transform))
    case DictComp((k, v), comps) => DictComp(
      (k, transform(v)),
      comps.map(transform)
    )
    case SetComp(target, comps) => SetComp(
      transform(target),
      comps.map(transform)
    )
    case ListComp(target, comps) => ListComp(
      transform(target),
      comps.map(transform)
    )
    case GenComp(target, comps) => GenComp(
      transform(target),
      comps.map(transform)
    )
    case AwaitExpr(e) => AwaitExpr(transform(e))
  case YieldExpr(opt) => YieldExpr(opt.map(transform))
    case YieldFromExpr(e) => YieldFromExpr(transform(e))
    case CompExpr(head, lp) => CompExpr(
      transform(head),
      lp.map { case (op, e) => (op, transform(e)) }
    )
    case Call(expr1, le, lk) => Call(
      transform(expr1),
      le.map(transform),
      lk.map {
        case NormalKwarg(id, e) => NormalKwarg(id, transform(e))
        case DoubleStarredKwarg(e) => DoubleStarredKwarg(transform(e))
        case Keyword(opt, e) => Keyword(opt, transform(e))
      }
    )
    case FormattedValue(lhs, n, rhs) => FormattedValue(lhs, n, rhs)
    case JoinedStr(le) => JoinedStr(le)
    case EConst(e) => EConst(e)
    case Attribute(e, x) => Attribute(e, x)
    case Subscript(e, slice) => Subscript(transform(e), transform(slice))
    case Starred(e) => Starred(e)
    case EName(x) => EName(x)
    case Slice(begin, end, step) => Slice(
      begin.map(transform),
      end.map(transform),
      step.map(transform)
    )
    case GroupExpr(e) => GroupExpr(transform(e))
  }

  /////////////////////////////////////////
  // transformers for sub-constructs
  /////////////////////////////////////////
  def transform(comp: Comprehension)(implicit env: Env): Comprehension = 
    comp match {
      case Compre(target, in, conds) =>
        Compre(target, transform(in), conds.map(transform))
      case AsyncCompre(target, in, conds) =>
        AsyncCompre(target, transform(in), conds.map(transform))
    }

  def transform(handler: ExcHandler)(
    implicit env: Env
  ): (ExcHandler, List[Warning]) =
    handler match {
      case ExcHandler(except, idOpt, body) =>
        val (newHandler, _, lw) = transform(body)
        (ExcHandler(except, idOpt, newHandler), lw)
    }

  def transform(al: List[Alias])(implicit env: Env): Env = 
    al.foldLeft(env)((e, a) => transform(a)(e))

  def transform(alias: Alias)(implicit env: Env): Env = env

  // name changed because of same type after type erasure
  def transformWithList(wl: List[WithItem])(implicit env: Env): (List[WithItem], Env) = 
    wl.foldRight(List[WithItem](), env) {
      case (w, (lw, e)) =>
        val (wTrans, eTrans) = transform(w)(e)
        (wTrans :: lw, eTrans)
    }

  def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
    case WithItem(e, opt) => (WithItem(transform(e), opt), env)
  }
  
  def transform(item: DictItem)(implicit env: Env): DictItem = item match {
    case KVPair(k, v) => KVPair(k, transform(v))
    case DoubleStarred(e) => DoubleStarred(e)
  }

  def transform(mc: MatchCase)(
    implicit env: Env
  ): (MatchCase, List[Warning]) = mc match {
    case MatchCase(pat, cond, body) =>
      val (newBody, _, lw) = transform(body)
      (MatchCase(transform(pat), cond.map(transform), newBody), lw)
  }

  def transform(pat: Pattern)(implicit env: Env): Pattern = pat match {
    case MatchValue(e) => MatchValue(transform(e))
    case MatchSingleton(c) => MatchSingleton(c)
    case MatchSeq(lpat) => MatchSeq(lpat.map(transform))
    case MatchStar(opt) => MatchStar(opt)
    case MatchMapping(map, opt) => MatchMapping(
      map.map { case (e, pat) => (e, transform(pat)) },
      opt
    )
    case MatchClass(e, lpat, map) => MatchClass(e,
      lpat.map(transform),
      map.map { case (k, v) => (k, transform(v)) }
    )
    case MatchAs(opt, x) => MatchAs(opt.map(transform), x)
    case MatchOr(lpat) => MatchOr(lpat.map(transform))
    case MatchWildcard => MatchWildcard
    case MatchGroup(p) => MatchGroup(transform(p))
  }
  /////////////////////////////////////////
  // implicit conversion for Stmt
  /////////////////////////////////////////
  implicit def stmt2stmts(stmt: Stmt): List[Stmt] = List(stmt)
  implicit def warning2Warnings(warning: Warning): List[Warning] =
    List(warning)
  implicit def addWarning[T](returnValue: T): (T, List[Warning]) =
    (returnValue, Nil)
  implicit def addWarning[T <% List[Stmt]](
    returnValue: (T, Env)
  ): (List[Stmt], Env, List[Warning]) =
    (returnValue._1, returnValue._2, Nil)
}

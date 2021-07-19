package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Useful._

trait Transformer {
  // transformed one AST into another AST
  def apply(ast: Node): Node = ???

  def transform(ast: Node): Node = ast match {
    case Module(body, tyIgnore) => Module(transform(body)(Env())._1, tyIgnore)
  }

  def transform(stmts: List[Stmt])(
    implicit env: Env
  ): (List[Stmt], Env) = stmts.foldLeft((List[Stmt](), Env())) {
    case ((stmtList, env), stmt) =>
      val (newStmtList, newEnv) = transform(stmt)(env)
      (stmtList ++ newStmtList, newEnv)
  }

  // TODO refactor divide into List[Stmt] and [Stmt] returning versions
  def transform(stmt: Stmt)(
    implicit env: Env
  ) : (List[Stmt], Env) = stmt match {
    // function def
    case FunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(FunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    case AsyncFunDef(decos, name, args, retTy, tyExpr, body) =>
      (List(AsyncFunDef(decos, name, args, retTy, tyExpr, transform(body)(env)._1)), env) 
    // class def
    case ClassDef(decos, name, exprs, kwds, body) =>
      (List(ClassDef(decos, name, exprs, kwds, transform(body)(env)._1)), env)
    // return, del
    case ReturnStmt(eopt) => (List(ReturnStmt(eopt.map(expr => transform(expr)(env)))), env)
    case DelStmt(tl) => (List(DelStmt(tl)), env)
    // strict form of assignment
    case AssignStmt(ts, expr, ty) => ???
    // other form of assignment
    case AugAssign(lhs, bop, rhs) => (List(AugAssign(lhs, bop, transform(rhs)(env))), env)
    case AnnAssign(t, ann, e) => ???
    // for statement
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (List(ForStmt(ty, forExpr, transform(inExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (List(AsyncForStmt(ty, forExpr, transform(inExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    // while statement
    case WhileStmt(wExpr, doStmt, elseStmt) =>
      (List(WhileStmt(transform(wExpr)(env), transform(doStmt)(env)._1, transform(elseStmt)(env)._1)), env) 
    // if statement
    case IfStmt(cond, thenStmt, elseStmt) =>
      (List(IfStmt(transform(cond)(env), transform(thenStmt)(env)._1, transform(elseStmt)(env)._1)), env)
    // with statement
    case WithStmt(ty, items, doStmt) => ???
    case AsyncWithStmt(ty, items, doStmt) => ???
    // match statement
    case MatchStmt(expr, cases) =>
      (List(MatchStmt(transform(expr)(env), cases.map(c => transform(c)(env)))), env)  
    // exception-related statements
    case RaiseStmt(expr, from) =>
      (List(RaiseStmt(expr, from)), env)
    case TryStmt(tryStmt, handlers, elseStmt, finallyStmt) =>
      val newTryStmt =
        TryStmt(
          transform(tryStmt)(env)._1, handlers.map(transform),
          transform(elseStmt)(env)._1, transform(finallyStmt)(env)._1)
      (List(newTryStmt), env)
    case AssertStmt(expr, toRaise) =>
      (List(AssertStmt(transform(expr)(env), toRaise)), env)
    // module, scope related statements
    case ImportStmt(al) =>
      val newEnv = transform(al)
      val diffEnv = newEnv \ env
      diffEnv.get("tensor_flow") match {
        case Some(x) if diffEnv.size == 1 => (
          ImportStmt(al) ::
          parseStmts(s"""
            import horovod.tensorflow as hvd
            hvd_broadcast_done = False
            hvd_init()
            gpus = $x.config.experimental.list_pysical_devices('GPU')
            for gpu in gpus:
              $x.config.expreimental.set_memory_growth(gpu, True)
            if gpus:
              $x.config.experimental.\\
                set_visible_devices(gpus[hvd.local_rank()], 'GPU')
            """),
          env)
        case _ => (List(ImportStmt(al)), newEnv)
      }
    case ImportFromStmt(lv, fromId, al) =>
      (List(ImportFromStmt(lv, fromId, al)), env)
    case GlobalStmt(il) => (List(GlobalStmt(il)), env)
    case NonlocalStmt(il) => (List(NonlocalStmt(il)), env)
    // strict form of expr
    case ExprStmt(Call(f, le, lk)) => (env.get("optimizer"), f) match {
      case (Some(x), Attribute(EName(fname), Id("apply_gradients")))
        if x == fname => 
          val newid = newId
          findKwarg(lk, "grads_and_vars") match {
            case Some(kwarg) => (
              parseStmts(s"""
                $newid = ${beautify(kwarg.expr)}
              """) ++ (
              ExprStmt(Call(f, le,
                replaceElement(lk, kwarg, kwarg.copy(expr = Id(newid)))
              )) ::
              parseStmts(s"""
                global hvd_broadcast_done
                if not hvd_broadcast_done:
                  hvd.broadcast_variables(
                    [x[1] for x in $newid],
                    root_rank=0
                  )
                  hvd.broadcast_variables(
                    optimizer.variables(),
                    root_rank=0
                  )
                  hvd_broadcast_done = True
                """)),
              env)
            case None => (
              // TODO: assert le is nonempty
              parseStmts(s"""
                $newid = ${beautify(le.head)}
              """) ++ (
              Call(f, Id(newid) :: le.tail, lk) ::
              parseStmts(s"""
                global hvd_broadcast_done
                if not hvd_broadcast_done:
                  hvd.broadcast_variables(
                    [x[1] for x in $newid],
                    root_rank=0
                  )
                  hvd.broadcast_variables(
                    optimizer.variables(),
                    root_rank=0
                  )
                  hvd_broadcast_done = True
              """)),
              env)
          }
      case _ => (List(ExprStmt(transform(Call(f, le, lk)))), env)
    }
    // general form of expr
    case ExprStmt(e) => (List(ExprStmt(transform(e))), env)
    case PassStmt => (List(PassStmt), env)
    case BreakStmt => (List(BreakStmt), env)
    case ContinueStmt => (List(ContinueStmt), env)
    // TODO: check transform from simple to compound exists
    case OnelineStmt(ls) =>
      val (newLs, newEnv) = transform(ls)
      (List(OnelineStmt(newLs)), newEnv)
  }

  def transform(expr: Expr)(
    implicit env: Env
  ): Expr = expr match {
    case BoolExpr(op, lhs, rhs) =>
      BoolExpr(op, transform(lhs), transform(rhs))
    case NamedExpr(lhs, rhs) => NamedExpr(lhs, transform(rhs))
    case BinaryExpr(op, lhs, rhs) =>
      BinaryExpr(op, transform(lhs), transform(rhs))
    case UnaryExpr(op, e) => UnaryExpr(op, transform(e))
    case LambdaExpr(args, e) => LambdaExpr(args, transform(e))
    case IfExpr(e, cond, ee) =>
      IfExpr(transform(e),transform(cond),transform(ee))
    case DictExpr(map, dstar) => DictExpr(
      map.map { case (k, v) => (k, transform(v)) },
      dstar
    )
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
    case Call(f, le, kwds) => (env.get("dataset"), f) match {
      case (Some(x), Attribute(EName(fname), Id("take"))) =>
        val containsCountKwarg = kwds.exists {
          case Kwarg(Some(Id("count")), _) => true
          case _ => false
        }
        // TODO: handle comment!!
        val (newLe, newKwds) = (???, ???)
        Call(f, newLe, newKwds)
      case _ => Call(
        transform(f),
        le.map(transform),
        kwds.map { case Kwarg(opt, e) => Kwarg(opt, transform(e)) }
      )
    }
    case FormattedValue(lhs, n, rhs) => FormattedValue(lhs, n, rhs)
    case JoinedStr(le) => JoinedStr(le)
    case EConst(e) => EConst(e)
    case Attribute(e, x) => Attribute(e, x)
    case Subscript(e, slice) => Subscript(transform(e), transform(slice))
    case Starred(e) => Starred(e)
    case DoubleStarred(e) => DoubleStarred(e)
    case EName(x) => EName(x)
    case Slice(begin, end, step) => Slice(
      begin.map(transform),
      end.map(transform),
      step.map(transform)
    )
    case GroupExpr(e) => e
  }

  def transform(comp: Comprehension)(
    implicit env: Env
  ): Comprehension = comp match {
    case Compre(target, in, conds) =>
      Compre(target, transform(in), conds.map(transform))
    case AsyncCompre(target, in, conds) =>
      AsyncCompre(target, transform(in), conds.map(transform))
  }

  def transform(handler: ExcHandler)(
    implicit env: Env
  ): ExcHandler = handler match {
    case ExcHandler(except, idOpt, body) =>
      ExcHandler(except, idOpt, transform(body)._1)
  }

  def transform(al: List[Alias])(
    implicit env: Env
  ): Env = al.foldLeft(env)((e, a) => transform(a)(e))
  def transform(alias: Alias)(
    implicit env: Env
  ): Env = alias match {
    case Alias(List(x), None) if x.name == "tensorflow" =>
      env.add("tensor_flow", x)
    case Alias(List(x), Some(as)) if x.name == "tensorflow" =>
      env.add("tensor_flow", as)
    case _ => env
  }

  // name changed because of same type after type erasure
  def transformWithList(wl: List[WithItem])(
    implicit env: Env
  ): (List[WithItem], Env) = wl.foldRight(List[WithItem](), env) {
    case (w, (lw, e)) =>
      val (wTrans, eTrans) = transform(w)(e)
      (wTrans :: lw, eTrans)
  }
  def transform(w: WithItem)(
    implicit env: Env
  ): (WithItem, Env) = w match {
    case WithItem(e, None) => (WithItem(transform(e), None), env)
    case WithItem(e, Some(asE)) => (env.get("tensor_flow"), e, asE) match {
      case (
        Some(x),
        Call(Attribute(EName(f), Id("GradientTape")), Nil, Nil),
        EName(asX)
      ) if x == f =>
          (WithItem(e, Some(asE)), env.add("gradient_tape", asX))
      case _ => (WithItem(transform(e), Some(asE)), env)
      }
  }

  def transform(mc: MatchCase)(
    implicit env: Env
  ): MatchCase = mc match {
    case MatchCase(pat, cond, body) =>
      MatchCase(transform(pat), cond.map(transform), transform(body)._1)
  }

  def transform(pat: Pattern)(
    implicit env: Env
  ): Pattern = pat match {
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
    case MatchGroup(p) => p
  }

  def parseStmts(code: String): List[Stmt] = {
    TokenListParser(tokenizeText(code))

  }

  def newId: String = ???
  def findKwarg(lk: List[Kwarg], str: String): Option[Kwarg] =
    lk.find {
      case Kwarg(Some(Id(x)), _) if x == str => true
      case _ => false
    }
  def replaceElement[T](lk: List[T], from: T, to: T): List[T] = {
    lk.zipWithIndex.find(e => e._1 == from) match {
      case Some((e, index)) =>
        lk.slice(0, index) ++ (to :: lk.slice(index + 1, lk.length))
      case None => lk
    }
  }
  implicit def expr2stmt(e: Expr): Stmt = ExprStmt(e)
  implicit def id2expr(x: Id): Expr = EName(x)
}

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
    // id_r = expr_1(le, lk) (# type: s)?
    case AssignStmt(targets, Call(expr1, le, lk), ty)
      if targets.size == 1 && targets.head.isInstanceOf[EName] =>
        val EName(idr) = targets.head
        expr1 match {
          // σ("tensor_flow") = id_t and expr_1 = id_t.data.Dataset.expr_3
          // TODO: is expr_3 means id?
          case Attribute(Attribute(Attribute(
            EName(idt), Id("data")), Id("Dataset")), _)
            if env.get("tensor_flow") contains idt =>
              // id_r = expr_1(le, lk) (# type: s)?
              // TODO: where is id_1?
              (AssignStmt(targets, Call(expr1, le, lk), ty),
                env.add("dataset", idr))
          // σ("tensor_flow") = id_t and expr_1 = id_t.optimizer.Adam
          case Attribute(Attribute(
            EName(fid), Id("optimizer")), Id("Adam")) =>
              findKwarg(lk, "learning_rate") match {
                // id_i = learning_rate when 1 <= i <= k
                case Some(kwarg) =>
                  val expr2i = kwarg.expr
                  val newLk = replaceElement(lk, kwarg, kwarg.copy(
                    // TODO: check precidence
                    expr = parseExpr(s"${beautify(expr2i)} * hvd.size()")
                  ))
                  // id_r = expr1(le, lk[expr_2i -> expr_2i * hvd.size()])
                  //   (# type: s)?
                  (AssignStmt(targets, Call(expr1, le, newLk), ty),
                    env.add("optimizer", idr))
                // such id_i doesn't exist
                case None =>
                  // id_r = expr1(le[expr_11 -> expr_11 * hvd.size()], lk)
                  //   (# type: s)?
                  (AssignStmt(targets, Call(expr1,
                    parseExpr(s"${beautify(le.head)} * hvd.size()") ::
                    le.tail, lk), ty), env.add("optimizer", idr))
              }
          // σ("optimizer") = id_t and expr = id_t.apply_gradients
          case Attribute(EName(idt), Id("apply_gradients"))
            if env.get("optimizer") contains idt =>
              val idz = newId
              findKwarg(lk, "grads_and_vars") match {
                // id_i = grads_and_vars when 1 <= i <= k
                case Some(kwarg) =>
                  val expr2i = kwarg.expr
                  val newLk = replaceElement(lk, kwarg,
                    kwarg.copy(expr = EName(idz)))
                  (
                    // id_z = expr_2i
                    AssignStmt(List(EName(idz)), expr2i) ::
                    // id_r = expr_1(le, lk[expr_2i -> id_z]) (# type: s)?
                    AssignStmt(targets, Call(expr1, le, newLk), ty) ::
                    parseStmts(s"""
                      global hvd_broadcast_done
                      if not hvd_broadcast_done:
                        hvd.broadcast_variables(
                          [x[1] for x in ${idz.name}],
                          root_rank=0
                        )
                        hvd_broadcast_done = True
                    """),
                    env
                  )
                // such id_i doesn't exist
                case None => (
                  // TODO: assert le is nonempty
                  // id_z = expr_11
                  AssignStmt(List(EName(idz)), le.head) ::
                  // id_r = expr_1(le[expr_11 -> id_z], lk) (# type: s)?
                  AssignStmt(targets, Call(expr1,
                    EName(idz) :: le.tail, lk), ty) ::
                  parseStmts(s"""
                    global hvd_broadcast_done
                    if not hvd_broadcast_done:
                      hvd.broadcast_variables(
                        [x[1] for x in ${idz.name}],
                        root_rank=0
                      )
                      hvd_broadcast_done = True
                  """),
                  env
                )
              }
          case _ => (
            // id_r = TRANS(expr_1(le, lk))(σ) (# type: s)?
            AssignStmt(targets, transform(Call(expr1, le, lk)), ty),
            env
          )
        }
    // general form of assignment
    case AssignStmt(targets, e, ty) =>
      (List(AssignStmt(targets, transform(e), ty)), env)
    case AugAssign(lhs, bop, rhs) => (List(AugAssign(lhs, bop, transform(rhs)(env))), env)
    case AnnAssign(expr1, expr2, expr3) =>
      (expr1, expr3) match {
        // expr_1 = id_1 and σ("tensor_flow") = id_2 and
        //   expr_3 = id_2.data.Dataset.expr_4(le, lk)
        //   TODO: expr_4 id?
        case (EName(id1), Some(Call(Attribute(Attribute(Attribute(
          EName(id2), Id("data")), Id("Dataset")), _), le, lk)))
          if env.get("tensor_flow") contains id2 =>
            // TODO: type comment?
            ???
        case _ => ???
      }
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
    case WithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = env \ tempEnv
      diffEnv.get("gradient_tape") match {
        // σ1 \ σ = ["gradient_tape" -> id]
        case Some(id) if diffEnv.size == 1 => (
          WithStmt(ty, newItems, newStmts) ::
          parseStmts(s"""
            ${id.name} = hvd.DistributedGradientTape(${id.name})
          """),
          newEnv)
        case _ => (WithStmt(ty, newItems, newStmts), newEnv)
      }
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = env \ tempEnv
      diffEnv.get("gradient_tape") match {
        // σ1 \ σ = ["gradient_tape" -> id]
        case Some(id) if diffEnv.size == 1 => (
          AsyncWithStmt(ty, newItems, newStmts) ::
          parseStmts(s"""
            ${id.name} = hvd.DistributedGradientTape(${id.name})
          """),
          newEnv)
        case _ => (AsyncWithStmt(ty, newItems, newStmts), newEnv)
      }
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
    case ImportStmt(alias) =>
      val newEnv = transform(alias)
      val diffEnv = newEnv \ env
      diffEnv.get("tensor_flow") match {
        // σ1 \ σ = ["tensor_flow" -> id]
        case Some(id) if diffEnv.size == 1 => (
          // import alias
          ImportStmt(alias) ::
          parseStmts(s"""
            import horovod.tensorflow as hvd
            hvd_broadcast_done = False
            hvd_init()
            gpus = ${id.name}.config.experimental.list_pysical_devices('GPU')
            for gpu in gpus:
              ${id.name}.config.expreimental.set_memory_growth(gpu, True)
            if gpus:
              ${id.name}.config.experimental.\\
                set_visible_devices(gpus[hvd.local_rank()], 'GPU')
          """),
          newEnv)
        case _ =>
          // import alias
          (ImportStmt(alias), newEnv)
      }
    case ImportFromStmt(lv, fromId, al) =>
      (List(ImportFromStmt(lv, fromId, al)), env)
    case GlobalStmt(il) => (List(GlobalStmt(il)), env)
    case NonlocalStmt(il) => (List(NonlocalStmt(il)), env)
    // strict form of expr
    // expr1(le, lk)
    case ExprStmt(Call(expr1, le, lk)) => expr1 match {
      // σ("optimizer") = id_t and expr_1 = id_t.apply_gradients
      case Attribute(EName(idt), Id("apply_gradients"))
        if env.get("optimizer") contains idt =>
          val idz = newId
          findKwarg(lk, "grads_and_vars") match {
            // id_i = grads_and_vars when 1 <= i <= k
            case Some(kwarg) =>
              val expr2i = kwarg.expr
              val newLk = replaceElement(lk, kwarg,
                kwarg.copy(expr = EName(idz)))
            (
              // id_z = expr_2i
              AssignStmt(List(EName(idz)), expr2i) ::
              // expr_1(le, lk[expr_2i -> id_z])
              ExprStmt(Call(expr1, le, newLk)) ::
              parseStmts(s"""
                global hvd_broadcast_done
                if not hvd_broadcast_done:
                  hvd.broadcast_variables(
                    [x[1] for x in ${idz.name}],
                    root_rank=0
                  )
                  hvd.broadcast_variables(
                    optimizer.variables(),
                    root_rank=0
                  )
                  hvd_broadcast_done = True
              """),
              env)
            case None => (
              // TODO: assert le is nonempty
              // id_z = expr_11
              AssignStmt(List(EName(idz)), le.head) ::
              // expr1(le[expr_11 -> id_z], lk)
              ExprStmt(Call(expr1, EName(idz) :: le.tail, lk)) ::
              parseStmts(s"""
                global hvd_broadcast_done
                if not hvd_broadcast_done:
                  hvd.broadcast_variables(
                    [x[1] for x in ${idz.name}],
                    root_rank=0
                  )
                  hvd.broadcast_variables(
                    optimizer.variables(),
                    root_rank=0
                  )
                  hvd_broadcast_done = True
              """),
              env)
          }
      case _ =>
        // TRANS(expr1(le, lk))(σ)
        (ExprStmt(transform(Call(expr1, le, lk))), env)
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
  def parseExpr(str: String): Expr = {
    val stmts = parseStmts(str)
    stmts.headOption match {
      case Some(ExprStmt(e)) if stmts.size == 1 => e
      case _ => ???
    }
  }

  def newId: Id = ???
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
  implicit def stmt2stmts(stmt: Stmt): List[Stmt] = List(stmt)
}

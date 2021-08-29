package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Preprocess._
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Transformer {
  // transformed one AST into another AST
  def apply(ast: Module): Module = ast match {
    case m @ Module(body, tyIg, name) =>
      // print summary
      val summary = TrainingLoop(m)
      println
      print(s"$CYAN<${name.get}> $RESET: ")
      println(summary)
      summary.tl match {
        case GradTape => Module(transform(body)(Env())._1, tyIg, name)
        case _ => m
      }
  }

  /////////////////////////////////////////
  // transformer for statements
  /////////////////////////////////////////
  def transform(stmts: List[Stmt])(implicit env: Env): (List[Stmt], Env) = 
    stmts.foldLeft((List[Stmt](), env)) {
      case ((stmtList, e), stmt) =>
        val (newStmtList, newEnv) = transform(stmt)(e)
        (stmtList ++ newStmtList, newEnv)
    }

  def transform(stmt: Stmt)(implicit env: Env): (List[Stmt], Env) = stmt match {
    // function def
    case FunDef(decos, name, args, retTy, tyExpr, body) =>
      (FunDef(decos, name, args, retTy, tyExpr, transform(body)._1), env) 
    case AsyncFunDef(decos, name, args, retTy, tyExpr, body) =>
      (AsyncFunDef(decos, name, args, retTy, tyExpr, transform(body)._1), env) 

    // class def
    case ClassDef(decos, name, exprs, kwds, body) =>
      (ClassDef(decos, name, exprs, kwds, transform(body)._1), env)

    // return, del
    case ReturnStmt(eopt) =>
      (ReturnStmt(eopt.map(expr => transform(expr))), env)
    case DelStmt(tl) => (DelStmt(tl), env)

    /////////////////////////////////////////////////////////////////
    //// strict form of assignment
    /////////////////////////////////////////////////////////////////
    case AssignStmt(targets, Call(expr1, exprs, kwds), ty)
      if (targets.size == 1 && targets.head.isInstanceOf[EName]) =>
        val EName(idr) = targets.head
        expr1 match {
          // case 1) "tensor_flow" -> data.Dataset
          case Attribute(Attribute(Attribute(EName(idt), Id("data")), Id("Dataset")), _)
            if env.get("tensor_flow") contains Id(idt.name) =>
              (AssignStmt(targets, Call(expr1, exprs, kwds), ty), 
                env.add("dataset", idr))

          // case 2) "tensor_flow" -> train.Checkpoint
          case Attribute(Attribute(EName(idt), Id("train")), Id("Checkpoint"))
            if env.get("tensor_flow") contains Id(idt.name) =>
              (AssignStmt(targets, Call(expr1, exprs, kwds), ty), 
                env.add("checkpoint", idr))

          // case 3) "tensor_flow" -> optimizers.Adam 
          case Attribute(Attribute(EName(idt), Id("optimizers")), Id("Adam"))
            if env.get("tensor_flow") contains Id(idt.name) =>
              // find id_i "learning_rate"
              findKwarg(kwds, "learning_rate") match {
                case Some(kwarg) =>
                  val expr2i = kwarg.expr
                  val newkwds = replaceElement(kwds, kwarg, kwarg.copy(
                    expr = parseExpr(s"(${beautify(expr2i)}) * hvd.size()")
                  ))
                  (AssignStmt(targets, Call(expr1, exprs, newkwds), ty), 
                    env.add("optimizer", idr))
                // such id_i doesn't exist
                case None =>
                  val newexprs = 
                    List(parseExpr(s"(${beautify(exprs.head)}) * hvd.size()")) ++
                    exprs.tail
                  (AssignStmt(targets, Call(expr1, newexprs, kwds), ty), 
                    env.add("optimizer", idr))
              }

          // case 3) "optimizer" -> apply_gradients
          case Attribute(EName(idt), Id("apply_gradients"))
            if env.get("optimizer") contains idt =>
              val idz = newId
              // find id_i "grads_and_vars"
              findKwarg(kwds, "grads_and_vars") match {
                case Some(kwarg) =>
                  val expr2i = kwarg.expr
                  val newkwds = replaceElement(kwds, kwarg,
                    kwarg.copy(expr = EName(idz)))
                  val newStmts = List(
                    AssignStmt(List(EName(idz)), expr2i),
                    AssignStmt(targets, Call(expr1, exprs, newkwds), ty),
                  ) ++ parseStmts(stmtData("assign-optimizer-some")(List(idz.name)))
                  (newStmts, env) 
                // such id_i doesn't exist
                case None => 
                  // idz == expr_11
                  val newStmts = List(
                    AssignStmt(List(EName(idz)), exprs.head),
                    AssignStmt(targets, Call(expr1, EName(idz)::exprs.tail, kwds), ty)
                  ) ++ parseStmts(stmtData("assign-optimizer-none")(List(idz.name)))
                  (newStmts, env)
              }
          // case 4) "chcekpoint" -> idt.save
          case Attribute(EName(idt), Id("save"))
            if env.get("checkpoint") contains idt =>
              // hvd.rank()
              val rankExpr = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
              // hvd.rank() == 0
              val condExpr = CompExpr(rankExpr, List((CEq,EConst(IntLiteral(0)))))
              // if hvd.rank() == 0: ...
              val newIfStmt = IfStmt(condExpr, List(stmt), Nil)
              (newIfStmt, env)

          // case 5) etc.
          case _ =>
            (AssignStmt(targets, transform(Call(expr1, exprs, kwds)), ty), env)
        }
    /////////////////////////////////////////////////////////////////
    // general form of assignment
    /////////////////////////////////////////////////////////////////

    // AssignStmt that targets is non-singular or non-id
    case AssignStmt(targets, e, ty) => 
      (AssignStmt(targets, transform(e), ty), env)
    // AugAssign case
    case AugAssign(lhs, bop, rhs) => (AugAssign(lhs, bop, transform(rhs)), env)
    // AnnAssign case: 
    case AnnAssign(e1, e2, e3) =>
      (e1, e3) match {
        // case 1) "tensor_flow" -> Dataset case
        case (EName(id1), 
              Some(Call(Attribute(Attribute(Attribute(EName(id2), Id("data")), Id("Dataset")), _), le, lk)))
          if env.get("tensor_flow") contains id2 => (
            AnnAssign(e1, e2, e3),
            env.add("dataset", id1)
          )
        // case 2) otherwise
        case _ => (AnnAssign(e1, e2, e3.map(transform)), env)
      }


    /////////////////////////////////////////////////////////////////
    // for statement
    case ForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (ForStmt(ty, forExpr, transform(inExpr), transform(doStmt)._1, transform(elseStmt)._1), env)
    case AsyncForStmt(ty, forExpr, inExpr, doStmt, elseStmt) =>
      (AsyncForStmt(ty, forExpr, transform(inExpr), transform(doStmt)._1, transform(elseStmt)._1), env)
    // while statement
    case WhileStmt(wExpr, doStmt, elseStmt) =>
      (WhileStmt(transform(wExpr), transform(doStmt)._1, transform(elseStmt)._1), env) 
    // if statement
    case IfStmt(cond, thenStmt, elseStmt) =>
      (IfStmt(transform(cond), transform(thenStmt)._1, transform(elseStmt)._1), env)

    /////////////////////////////////////////////////////////////////
    // with statement
    /////////////////////////////////////////////////////////////////
    case WithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      // get "gradient_tape" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts = 
            List(WithStmt(ty, newItems, newStmts)) ++ 
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv)
        // not found
        case _ => (WithStmt(ty, newItems, newStmts), newEnv)
      }
    /////////////////////////////////////////////////////////////////
    // Async with statement
    /////////////////////////////////////////////////////////////////
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, tempEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(tempEnv)
      val diffEnv = tempEnv \ env
      // get "gradient_tap" id
      diffEnv.get("gradient_tape") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newerStmts =
            List(AsyncWithStmt(ty, newItems, newStmts)) ++
            parseStmts(s"${id.name} = hvd.DistributedGradientTape(${id.name})")
          (newerStmts, newEnv)
        case _ => (AsyncWithStmt(ty, newItems, newStmts), newEnv)
      }

    /////////////////////////////////////////////////////////////////
    // match statement
    /////////////////////////////////////////////////////////////////
    case MatchStmt(expr, cases) =>
      (MatchStmt(transform(expr), cases.map(c => transform(c))), env)  

    // exception-related statements
    case RaiseStmt(expr, from) =>
      (RaiseStmt(expr, from), env)
    case TryStmt(tryStmt, handlers, elseStmt, finallyStmt) =>
      val newTryStmt =
        TryStmt(
          transform(tryStmt)._1, handlers.map(transform),
          transform(elseStmt)._1, transform(finallyStmt)._1)
      (newTryStmt, env)
    case AssertStmt(expr, toRaise) =>
      (AssertStmt(transform(expr), toRaise), env)

    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportStmt(alias) =>
      val newEnv = transform(alias)
      val diffEnv = newEnv \ env
      // get "tensor_flow" id 
      diffEnv.get("tensor_flow") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newStmts = List(ImportStmt(alias)) ++ 
            parseStmts(stmtData("import-some")(List(id.name))) 
          (newStmts, newEnv)
        // corresponding not found
        case _ => (ImportStmt(alias), newEnv)
      }

    /////////////////////////////////////////////////////////////////
    // other scope-related
    case ImportFromStmt(lv, fromId, al) => (ImportFromStmt(lv, fromId, al), env)
    case GlobalStmt(il) => (GlobalStmt(il), env)
    case NonlocalStmt(il) => (NonlocalStmt(il), env)

    /////////////////////////////////////////////////////////////////
    // strict form of expr stmts
    case ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      // case 1) func expr is "optimizer.apply_gradients"
      case Attribute(EName(idt), Id("apply_gradients")) 
        if env.get("optimizer") contains idt =>
          val idz = newId
          // get "grads_and_vars" id
          findKwarg(kwds, "grads_and_vars") match {
            // found 
            case Some(kwarg) =>
              val expr2i = kwarg.expr
              val newkwarg = kwarg.copy(expr = EName(idz))
              val newkwds = replaceElement(kwds, kwarg, newkwarg)
              val newStmts = List(
                AssignStmt(List(EName(idz)), expr2i),
                ExprStmt(Call(expr1, exprs, newkwds)),
              ) ++ parseStmts(stmtData("expr-optimizer-some")(List(idz.name, idt.name)))

              (newStmts, env)
            // not found
            case None => 
              val newStmts = List(
                AssignStmt(List(EName(idz)), exprs.head),
                ExprStmt(Call(expr1, EName(idz) :: exprs.tail, kwds)),
              ) ++ parseStmts(stmtData("expr-optimizer-none")(List(idz.name, idt.name)))
              (newStmts, env)
          }
      // case 2) print stmt
      case EName(Id("print")) => {
        // hvd.rank()
        val rank = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
        // hvd.rank() == 0 
        val condExpr = CompExpr(rank, List((CEq,EConst(IntLiteral(0)))))
        // if hvd.rank() == 0: ...
        val ifStmt = IfStmt(condExpr, List(stmt), Nil)
        (List(ifStmt), env)
      }
      // case 3) "checkpoint"
      case Attribute(EName(idt), Id("save"))
        if env.get("checkpoint") contains idt =>
          // hvd.rank()
          val rank = Call(Attribute(EName(Id("hvd")),Id("rank")), Nil, Nil)
          // hvd.rank() == 0 
          val condExpr = CompExpr(rank, List((CEq,EConst(IntLiteral(0)))))
          // if hvd.rank() == 0: ...
          val ifStmt = IfStmt(condExpr, List(stmt), Nil) 
          (ifStmt, env)
      // case _) other expr stmts
      case _ => (ExprStmt(transform(Call(expr1, exprs, kwds))), env)
    }

    /////////////////////////////////////////////////////////////////
    // general form of expr
    case ExprStmt(e) => (ExprStmt(transform(e)), env)
    case PassStmt => (PassStmt, env)
    case BreakStmt => (BreakStmt, env)
    case ContinueStmt => (ContinueStmt, env)
    // TODO: check transform from simple to compound exists
    case OnelineStmt(ls) =>
      val (newLs, newEnv) = transform(ls)
      (OnelineStmt(newLs), newEnv)
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
    case Call(expr1, le, lk) => expr1 match {
      // case 1) σ("dataset") = id_t and expr_1 = id_t.take
      case Attribute(EName(idt), Id("take")) 
        if env.get("dataset") contains idt =>
          findKwarg(lk, "count") match {
            case Some(kwarg) =>
              val expr2i = kwarg.expr
              val newLk = replaceElement(lk, kwarg,
                kwarg.copy(expr = parseExpr(s"${beautify(expr2i)} // hvd.size()")))
              Call(expr1, le, newLk)
            case None => Call(
              expr1,
              parseExpr(s"${beautify(le.head)} // hvd.size()") :: le.tail,
              lk
            )
          }
      
      // case _) else
      case _ => Call(
        transform(expr1),
        le.map(transform),
        lk.map {
          case NormalKwarg(id, e) => NormalKwarg(id, transform(e))
          case DoubleStarredKwarg(e) => DoubleStarredKwarg(transform(e))
        }
      )
    }
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

  def transform(handler: ExcHandler)(implicit env: Env): ExcHandler = 
    handler match {
      case ExcHandler(except, idOpt, body) =>
        ExcHandler(except, idOpt, transform(body)._1)
    }

  def transform(al: List[Alias])(implicit env: Env): Env = 
    al.foldLeft(env)((e, a) => transform(a)(e))

  def transform(alias: Alias)(implicit env: Env): Env = alias match {
    case Alias(List(x), None) if x.name == "tensorflow" =>
      env.add("tensor_flow", x)
    case Alias(List(x), Some(as)) if x.name == "tensorflow" =>
      env.add("tensor_flow", as)
    case _ => env
  }

  // name changed because of same type after type erasure
  def transformWithList(wl: List[WithItem])(implicit env: Env): (List[WithItem], Env) = 
    wl.foldRight(List[WithItem](), env) {
      case (w, (lw, e)) =>
        val (wTrans, eTrans) = transform(w)(e)
        (wTrans :: lw, eTrans)
    }

  def transform(w: WithItem)(implicit env: Env): (WithItem, Env) = w match {
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
  
  def transform(item: DictItem)(implicit env: Env): DictItem = item match {
    case KVPair(k, v) => KVPair(k, transform(v))
    case DoubleStarred(e) => DoubleStarred(e)
  }

  def transform(mc: MatchCase)(implicit env: Env): MatchCase = mc match {
    case MatchCase(pat, cond, body) =>
      MatchCase(transform(pat), cond.map(transform), transform(body)._1)
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
  // helper functions
  /////////////////////////////////////////
  def parseStmts(code: String): List[Stmt] = {
    TokenListParser(tokenizeText(code)).body
  }

  def parseExpr(str: String): Expr = {
    val stmts = parseStmts(str)
    stmts.headOption match {
      case Some(ExprStmt(e)) if stmts.size == 1 => e
      case _ => ???
    }
  }

  // TODO: need new id gen algorithm
  def newId: Id = Id("id_new")

  def findKwarg(lk: List[Kwarg], str: String): Option[NormalKwarg] =
    (lk.find {
      case NormalKwarg(Id(x), _) if x == str => true
      case _ => false
    }).asInstanceOf[Option[NormalKwarg]]

  def replaceElement[T](lk: List[T], from: T, to: T): List[T] = {
    lk.zipWithIndex.find(e => e._1 == from) match {
      case Some((e, index)) =>
        lk.slice(0, index) ++ (to :: lk.slice(index + 1, lk.length))
      case None => lk
    }
  }

  /////////////////////////////////////////
  // Data needed for transformation
  // TODO this is actually static thingy...
  /////////////////////////////////////////
  val stmtData: Map[String, List[String] => String] = Map(
    // strict assign
    "assign-optimizer-some" -> (names => {
        val name = names(0)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${name}],
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // strict assign
    "assign-optimizer-none" -> (names => { 
        val name = names(0)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${name}],
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // import stmt
    "import-some" -> (names => { 
        val name = names(0)
        s"""import horovod.tensorflow as hvd
           |hvd_broadcast_done = False
           |hvd.init()
           |gpus = ${name}.config.experimental.list_physical_devices('GPU')
           |for gpu in gpus:
           |  ${name}.config.experimental.set_memory_growth(gpu, True)
           |if gpus:
           |  ${name}.config.experimental.\\
           |    set_visible_devices(gpus[hvd.local_rank()], 'GPU')""".stripMargin
    }),
    // expression stmt
    "expr-optimizer-some" -> (names => {
        val idz = names(0)
        val idt = names(1)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${idz}],
           |    root_rank=0
           |  )
           |  hvd.broadcast_variables(
           |    ${idt}.variables(),
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
    // expression stmt
    "expr-optimizer-none" -> (names => { 
        val idz = names(0)
        val idt = names(1)
        s"""global hvd_broadcast_done
           |if not hvd_broadcast_done:
           |  hvd.broadcast_variables(
           |    [x[1] for x in ${idz}],
           |    root_rank=0
           |  )
           |  hvd.broadcast_variables(
           |    ${idt}.variables(),
           |    root_rank=0
           |  )
           |  hvd_broadcast_done = True""".stripMargin
    }),
  )

  /////////////////////////////////////////
  // implicit conversion for Stmt
  /////////////////////////////////////////
  implicit def stmt2stmts(stmt: Stmt): List[Stmt] = List(stmt)
}

package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser
import kr.ac.kaist.pyanalyzer.parser.Tokenizer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.transformer.Preprocess._
import kr.ac.kaist.pyanalyzer.transformer.TrainingLoop
import kr.ac.kaist.pyanalyzer.transformer.TransformerTape
import kr.ac.kaist.pyanalyzer.transformer.TransformerOptim
import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.Console._

object Transformer extends Transformer {
  // transformed one AST into another AST
  def apply(module: Module, env: Env = Env(), prompt: (String, String) => Unit): Module = {
    val summary = TrainingLoop(module)
    summary.tl match {
      case GradTape => TransformerTape(module, env, prompt)
      case Optimizer => TransformerOptim(module, env, prompt)
      case Bot => module.copy(body=transform(module.body)(env, prompt)._1)
    }
  }
}
trait TransformerMainScript extends Transformer {
  val writeMethods = List(
    "write", "summary", "save_weights", "load_weights", "save"
  ).map(Id(_))
  override def transform(stmt: Stmt)(
    implicit env: Env, prompt: (String, String) => Unit
  ): (List[Stmt], Env) = stmt match {
    case AssignStmt(List(EName(idr)), Call(expr1, exprs, kwds), ty) => expr1 match {
      case _ if env.isSubclass(expr1, "tensorflow.keras.models.Model") =>
        (stmt, env.add("model", idr))
      case _ if env.isSubclass(expr1, "tensorflow.keras.Model") =>
        (stmt, env.add("model", idr))
      case _ => super.transform(stmt)
    }
    case stmt @ ExprStmt(Call(expr1, exprs, kwds)) => expr1 match {
      case Attribute(EName(idt), id)
        if env.get("model").contains(idt) && writeMethods.contains(id) =>
          (getStmts("root-rank-wrapping", stmt), env)
      case Attribute(_, id) if writeMethods contains id =>
        prompt("Inaccurate transform", beautify(stmt))
        (getStmts("root-rank-wrapping", stmt), env)
      case EName(Id("print")) =>
        (getStmts("root-rank-wrapping", stmt), env)
      case Attribute(EName(idt), Id("print")) if env.get("tensor_flow") contains idt =>
        (getStmts("root-rank-wrapping", stmt), env)
      case _ => (ExprStmt(super.transform(Call(expr1, exprs, kwds))), env)
    }
    /////////////////////////////////////////////////////////////////
    // importstmt
    /////////////////////////////////////////////////////////////////
    case ImportStmt(alias) =>
      val classUpdatedEnv = transferStmt(env.getClassOrder)(stmt)
      val newEnv = transform(alias)(env.copy(classOrder = classUpdatedEnv))
      val diffEnv = newEnv \ env
      // get "tensor_flow" id 
      diffEnv.get("tensor_flow") match {
        // corresponding id found
        case Some(id) if diffEnv.size == 1 => 
          val newStmts = List(ImportStmt(alias)) ++ getStmts("import-some", id)
          (newStmts, newEnv)
        // corresponding not found
        case _ => (ImportStmt(alias), newEnv)
      }
    case ImportFromStmt(lv, fromId, al) =>
      val classUpdatedEnv = transferStmt(env.getClassOrder)(stmt)
      val newEnv = transform(al)(env.copy(classOrder = classUpdatedEnv))
      (ImportFromStmt(lv, fromId, al), newEnv)
    case _ =>
      val newEnv = env.copy(classOrder = transferStmt(env.getClassOrder)(stmt))
      super.transform(stmt)(newEnv, prompt)
  }

  def getStmts(name: String, nodes: Node*): List[Stmt] = getStmts(name, nodes.toList)
  def getStmts(name: String, nodes: List[Node]): List[Stmt] =
    codeData.get(name) match {
      case Some(data) => parseStmts(data(nodes.map(beautify(_))))
      case None => Nil
    }
  private val codeData: Map[String, List[String] => String] = Map(
    "root-rank-wrapping" -> (codeSeg => {
      val stmt = codeSeg(0)
      s"""if hvd.rank() == 0: $stmt"""
    }),
  )
}
trait Transformer {
  // transformed one AST into another AST

  /////////////////////////////////////////
  // transformer for statements
  /////////////////////////////////////////
  def transform(stmts: List[Stmt])(
    implicit env: Env, prompt: (String, String) => Unit
  ): (List[Stmt], Env) = 
    stmts.foldLeft((List[Stmt](), env)) {
      case ((stmtList, e), stmt) =>
        val (newStmtList, newEnv) = transform(stmt)(e, prompt)
        (stmtList ++ newStmtList, newEnv)
    }

  def transform(stmt: Stmt)(
    implicit env: Env, prompt: (String, String) => Unit
  ): (List[Stmt], Env) = stmt match {
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

    // for `os.environ['CUDA_VISIBLE_DEVICES']` case
    case AssignStmt(
      List(Subscript(Attribute(EName(idt), Id("environ")),
      EConst(StringLiteral("CUDA_VISIBLE_DEVICES")))), expr, ty) 
      if env.get("os") contains idt => 
        (List(), env)
    // AssignStmt that targets is non-singular or non-id
    case AssignStmt(targets, e, ty) => (AssignStmt(targets, transform(e), ty), env)
    // AugAssign case
    case AugAssign(lhs, bop, rhs) => (AugAssign(lhs, bop, transform(rhs)), env)
    // AnnAssign case: 
    case AnnAssign(e1, e2, e3) => (AnnAssign(e1, e2, e3.map(transform)), env)

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
      val (newItems, interEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(interEnv, prompt)
      (WithStmt(ty, newItems, newStmts), newEnv)
    case AsyncWithStmt(ty, items, doStmt) =>
      val (newItems, interEnv) = transformWithList(items)
      val (newStmts, newEnv) = transform(doStmt)(interEnv, prompt)
      (AsyncWithStmt(ty, newItems, newStmts), newEnv)

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
    implicit env: Env, prompt: (String, String) => Unit
  ): ExcHandler = 
    handler match {
      case ExcHandler(except, idOpt, body) =>
        ExcHandler(except, idOpt, transform(body)._1)
    }

  def transform(al: List[Alias])(implicit env: Env): Env = 
    al.foldLeft(env)((e, a) => transform(a)(e))

  // TODO
  def transform(alias: Alias)(implicit env: Env): Env = alias match {
    case Alias(List(x), None) if x.name == "tensorflow" =>
      env.add("tensor_flow", x)
    case Alias(List(x), Some(as)) if x.name == "tensorflow" =>
      env.add("tensor_flow", as)
    case Alias(List(x), None) if x.name == "keras" =>
      env.add("keras", x)
    case Alias(List(x), Some(as)) if x.name == "keras" =>
      env.add("keras", as)
    case Alias(List(x), None) if x.name == "optimizers" =>
      env.add("optimizers", x)
    case Alias(List(x), Some(as)) if x.name == "optimizers" =>
      env.add("optimizers", as)
    case Alias(List(x), None) if x.name == "os" =>
      env.add("os", x)
    case Alias(List(x), Some(as)) if x.name == "os" =>
      env.add("os", as)
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
    case WithItem(e, opt) => (WithItem(transform(e), opt), env)
  }
  
  def transform(item: DictItem)(implicit env: Env): DictItem = item match {
    case KVPair(k, v) => KVPair(k, transform(v))
    case DoubleStarred(e) => DoubleStarred(e)
  }

  def transform(mc: MatchCase)(
    implicit env: Env, prompt: (String, String) => Unit
  ): MatchCase = mc match {
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
  // implicit conversion for Stmt
  /////////////////////////////////////////
  implicit def stmt2stmts(stmt: Stmt): List[Stmt] = List(stmt)
}

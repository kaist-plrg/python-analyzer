package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.util.Useful._
import scala.sys.process._
import spray.json._

object AstJsonParser {
  val exePath = PY_AST_DIR + "/ast-json-extractor.py" 

  def fileToAst(path: String): Module = {
    val source = readFile(path)
    sourceToAst(source) match {
      case x: Module => x
      case _ => throw new RuntimeException("not a module")
    }
  }

  def sourceToAst(source: String): Node = {
    val astJson: JsObject = sourceToJson(source)
    val ast = parseJson(astJson)(AsModule)
    ast
  }

  def sourceToJson(source: String): JsObject = {
    val pyPath = PY_AST_DIR + "/temp.py" 
    val jsonPath = PY_AST_DIR + "/temp.json"

    // dump the source to temp.py
    writeFile(pyPath, source) 

    // execute external python to create temp.json
    Seq("python3", s"$exePath").!

    // read in the temp.json
    val astJson = try {
      val res = readFile(jsonPath)
      res.parseJson.asJsObject 
    } catch {
      // TODO better exception handling?
      case e: DeserializationException => 
        throw new RuntimeException("JsObject expected in top level")
    }    

    astJson
  }
  // parse wrapper
  trait As {
    type Output
    def apply(ast: JsObject): Output 
  }
  case object AsModule extends As {
    type Output = Module
    def apply(ast: JsObject): Output = parseModuleJson(ast)
  }
  case object AsStmt extends As {
    type Output = Stmt
    def apply(ast: JsObject): Output = parseStmtJson(ast) 
  }
  case object AsExpr extends As {
    type Output = Expr
    def apply(ast: JsObject): Output = parseExprJson(ast) 
  }
  case object AsBoolOp extends As {
    type Output = BoolOp
    def apply(ast: JsObject): Output = parseBoolOpJson(ast)
  }
  case object AsBinOp extends As {
    type Output = BinOp
    def apply(ast: JsObject): Output = parseBinOpJson(ast)
  }
  case object AsUnaryOp extends As {
    type Output = UnOp
    def apply(ast: JsObject): Output = parseUnaryOpJson(ast)
  }
  case object AsCompOp extends As {
    type Output = CompOp
    def apply(ast: JsObject): Output = parseCompOpJson(ast)
  }
  case object AsComprehension extends As {
    type Output = Comprehension
    def apply(ast: JsObject): Output = parseComprehensionJson(ast)
  } 
  case object AsExcHandler extends As {
    type Output = ExcHandler
    def apply(ast: JsObject): Output = parseExcHandlerJson(ast)
  }
  case object AsArgs extends As {
    type Output = Args
    def apply(ast: JsObject): Output = parseArgsJson(ast)
  }
  case object AsArg extends As {
    type Output = Arg
    def apply(ast: JsObject): Output = parseArgJson(ast)
  }
  case object AsKeyword extends As {
    type Output = Keyword
    def apply(ast: JsObject): Output = parseKeywordJson(ast)
  }
  case object AsAlias extends As {
    type Output = Alias
    def apply(ast: JsObject): Output = parseAliasJson(ast)
  }
  case object AsWithItem extends As {
    type Output = WithItem
    def apply(ast: JsObject): Output = parseWithItemJson(ast)
  }
  case object AsId extends As {
    type Output = Id
    def apply(ast: JsObject): Output = parseIdJson(ast)
  }
  case object AsConst extends As {
    type Output = Const
    def apply(ast: JsObject): Output = parseConstJson(ast)
  }
  def parseJson(ast: JsValue)(as: As): as.Output = ast match {
    case x: JsObject => as(x)
  }
  def parseJson(as: As): JsValue => as.Output = (x => x match {
    case x: JsObject => as(x)
  })

  //////////////////////////////////////////////////////////////////

  // parses top-level (Module) ast-json into Module case class
  def parseModuleJson(ast: JsObject): Module = 
    getJsObjectType(ast) match {
      case "Module" =>
        val body: List[JsValue] = ast.fields.get("body") match {
          case Some(JsArray(l)) => l.toList
        }
        Module(body.map(v => parseJson(v.asJsObject)(AsStmt)).toList)
    }

  // parses stmt ast-json
  def parseStmtJson(ast: JsObject): Stmt = 
    getJsObjectType(ast) match {
      case "FunctionDef" =>
        val name = applyToJsField(ast, "name", parseJson(AsId))
        val args = applyToJsField(ast, "args", parseJson(AsArgs))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val decos = applyToJsList(ast, "decorator_list", parseJson(AsExpr))
        val rets = applyToJsOption(ast, "returns", parseJson(AsExpr))
        val tyComment: Option[String] = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
        FunDef(decos, name, args, rets, tyComment, body) 
      case "AsyncFunctionDef" =>
        val name = applyToJsField(ast, "name", parseJson(AsId))
        val args = applyToJsField(ast, "args", parseJson(AsArgs))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val decos = applyToJsList(ast, "decorator_list", parseJson(AsExpr))
        val rets = applyToJsOption(ast, "returns", parseJson(AsExpr))
        val tyComment: Option[String] = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
        AsyncFunDef(decos, name, args, rets, tyComment, body)
      case "ClassDef" =>
        val name = applyToJsField(ast, "name", parseJson(AsId))
        val bases = applyToJsList(ast, "bases", parseJson(AsExpr))
        val kwds = applyToJsList(ast, "keywords", parseJson(AsKeyword))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val decos = applyToJsList(ast, "decorator_list", parseJson(AsExpr))
        ClassDef(decos, name, bases, kwds, body)
      case "Return" =>
        val value = applyToJsOption(ast, "value", parseJson(AsExpr))
        ReturnStmt(value)
      case "Delete" =>
        val targets = applyToJsList(ast, "targets", parseJson(AsExpr))
        DelStmt(targets)
      case "Assign" =>
        val targets = applyToJsList(ast, "targets", parseJson(AsExpr))
        val value = applyToJsField(ast, "value", parseJson(AsExpr))
        val tyComment = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
        AssignStmt(targets, value, tyComment) 
      case "AugAssign" =>
        val target = applyToJsField(ast, "target", parseJson(AsExpr))
        val op = applyToJsField(ast, "op", parseJson(AsBinOp))
        val value = applyToJsField(ast, "value", parseJson(AsExpr))
        AugAssign(target, op, value)
      case "AnnAssign" =>
        val target = applyToJsField(ast, "target", parseJson(AsExpr))
        val ann = applyToJsField(ast, "annotation", parseJson(AsExpr))
        val value = applyToJsOption(ast, "value", parseJson(AsExpr))
        AnnAssign(target, ann, value)
      case "For" =>
        val target = applyToJsField(ast, "target", parseJson(AsExpr))
        val iter = applyToJsField(ast, "iter", parseJson(AsExpr))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val orelse = applyToJsList(ast, "orelse", parseJson(AsStmt))
        val tyComment = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
        ForStmt(tyComment, target, iter, body, orelse)
      case "AsyncFor" =>
        val target = applyToJsField(ast, "target", parseJson(AsExpr))
        val iter = applyToJsField(ast, "iter", parseJson(AsExpr))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val orelse = applyToJsList(ast, "orelse", parseJson(AsStmt))
        val tyComment = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
        AsyncForStmt(tyComment, target, iter, body, orelse)  
      case "While" =>
        val test = applyToJsField(ast, "test", parseJson(AsExpr))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val orelse = applyToJsList(ast, "orelse", parseJson(AsStmt))
        WhileStmt(test, body, orelse)
      case "If" =>
        val test = applyToJsField(ast, "test", parseJson(AsExpr))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val orelse = applyToJsList(ast, "orelse", parseJson(AsStmt))
        IfStmt(test, body, orelse)
      case "With" =>
        val items = applyToJsList(ast, "items", parseJson(AsWithItem))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val tyComment = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
       WithStmt(tyComment, items, body) 
      case "AsyncWith" =>
        val items = applyToJsList(ast, "items", parseJson(AsWithItem))
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val tyComment = ast.fields("type_comment") match {
          case JsString(s) => Some(s)
          case JsNull => None
        }
       AsyncWithStmt(tyComment, items, body)
      case "Raise" =>
        val exception = applyToJsOption(ast, "exc", parseJson(AsExpr))
        val from = applyToJsOption(ast, "caese", parseJson(AsExpr))
        RaiseStmt(exception, from)
      case "Try" =>
        val body = applyToJsList(ast, "body", parseJson(AsStmt))
        val handlers = applyToJsList(ast, "handlers", parseJson(AsExcHandler))
        val orelse = applyToJsList(ast, "orelse", parseJson(AsStmt))
        val fbody = applyToJsList(ast, "finalbody", parseJson(AsStmt))
        TryStmt(body, handlers, orelse, fbody)
      case "Assert" =>
        val test = applyToJsField(ast, "test", parseJson(AsExpr))
        val msg = applyToJsOption(ast, "msg", parseJson(AsExpr))
        AssertStmt(test, msg)
      case "Import" =>
        val ns = applyToJsList(ast, "names", parseJson(AsAlias))
        ImportStmt(ns)
      case "ImportFrom" =>
        val mnames: List[Id] = ast.fields("module") match {
          case JsNull => Nil
          case x => parseJson(x.asJsObject)(AsId) match {
            case Id(s) => s.split(".").map(x => Id(x)).toList
          }
        }
        val anames = applyToJsList(ast, "names", parseJson(AsAlias))
        val level = ast.fields("level") match {
          case JsNumber(n) if n.isValidInt => n.intValue
        } 
        ImportFromStmt(level, mnames, anames)
      case "Global" =>
        val ns = applyToJsList(ast, "names", parseJson(AsId))
        GlobalStmt(ns)
      case "Nonlocal" =>  
        val ns = applyToJsList(ast, "names", parseJson(AsId))
        NonlocalStmt(ns)
      case "Expr" =>
        val exprObj = ast.fields("value").asJsObject
        val expr = parseExprJson(exprObj)
        ExprStmt(expr)
      case "Pass" => PassStmt 
      case "Break" => BreakStmt
      case "Continue" => ContinueStmt 
      case _ => throw new RuntimeException("Invalid Stmt object")
    }  
    
  // parseing expr ast-json
  def parseExprJson(ast: JsObject): Expr =
    getJsObjectType(ast) match {
      case "BoolOp" =>
        val op: BoolOp = applyToJsField(ast, "op", parseJson(AsBoolOp))
        val values = ast.fields("values") match {
          case JsArray(l) => l.map(v => parseExprJson(v.asJsObject)).toList
        }
        BoolGroupExpr(op, values)
      case "NamedExpr" =>
        val op: BinOp = applyToJsField(ast, "op", parseJson(AsBinOp))
        val target: Expr = applyToJsField(ast, "target", parseJson(AsExpr)) 
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr)) 
        NamedExpr(target, value)
      case "BinOp" =>
        val op: BinOp = applyToJsField(ast, "op", parseJson(AsBinOp)) 
        val lhs: Expr = applyToJsField(ast, "left", parseJson(AsExpr)) 
        val rhs: Expr = applyToJsField(ast, "right", parseJson(AsExpr)) 
        BinaryExpr(op, lhs, rhs) 
      case "UnaryOp" =>
        val op: UnOp = applyToJsField(ast, "unaryop", parseJson(AsUnaryOp)) 
        val expr: Expr = applyToJsField(ast, "operand", parseJson(AsExpr))
        UnaryExpr(op, expr)
      case "Lambda" =>
        val args: Args = applyToJsField(ast, "args", parseJson(AsArgs))
        val body: Expr = applyToJsField(ast, "body", parseJson(AsExpr)) 
        LambdaExpr(args, body)
      case "IfExp" =>
        val test: Expr = applyToJsField(ast, "test", parseJson(AsExpr))
        val body: Expr = applyToJsField(ast, "body", parseJson(AsExpr))
        val orelse: Expr = applyToJsField(ast, "orelse", parseJson(AsExpr)) 
        IfExpr(body, test, orelse) 
      case "Dict" =>
        val keys: List[Option[Expr]] = 
          applyToJsList(ast, "keys", v => v match {
            case JsNull => None
            case x => Some(parseExprJson(x.asJsObject))
          })
        val values: List[Expr] = 
          applyToJsList(ast, "values", v => parseExprJson(v.asJsObject)).toList 
        val zipped: List[(Option[Expr], Expr)] = keys zip values
        val (kvpairs: List[KVPair], dstars: List[DoubleStarred]) = 
          zipped.foldLeft((List[KVPair](), List[DoubleStarred]()))((acc, elm) => acc match {
              case (kl, dl) => elm match {
                case (None, e) => (kl, dl :+ DoubleStarred(e))
                case (Some(k), e) => (kl :+ KVPair(k, e), dl)
              } 
            })
        DictExpr(kvpairs ++ dstars)
      case "Set" =>
        val elts: List[Expr] = 
          applyToJsList(ast, "elts", parseJson(AsExpr)) 
        SetExpr(elts) 
      case "ListComp" =>
        val elt: Expr = applyToJsField(ast, "elt", parseJson(AsExpr))
        val gens: List[Comprehension] =
          applyToJsList(ast, "generators", parseJson(AsComprehension))
        ListComp(elt, gens)
      case "SetComp" =>
        val elt: Expr = applyToJsField(ast, "elt", parseJson(AsExpr))
        val gens: List[Comprehension] =
          applyToJsList(ast, "generators", parseJson(AsComprehension))
        SetComp(elt, gens)
      case "DictComp" =>
        val key: Expr = applyToJsField(ast, "key", parseJson(AsExpr))
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr))
        val gens: List[Comprehension] =
          applyToJsList(ast, "generators", parseJson(AsComprehension))
        DictComp((key, value), gens)
      case "GeneratorExp" =>
        val elt: Expr = applyToJsField(ast, "elt", parseJson(AsExpr))
        val gens: List[Comprehension] =
          applyToJsList(ast, "generators", parseJson(AsComprehension))
        GenComp(elt, gens) 
      case "Await" =>
        val value: Expr = applyToJsField(ast, "elt", parseJson(AsExpr))
        AwaitExpr(value)
      case "Yield" =>
        val value: Option[Expr] = ast.fields("value") match {
          case JsNull => None
          case x => Some(parseJson(x)(AsExpr))
        }
        YieldExpr(value)
      case "YieldFrom" =>
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr))
        YieldFromExpr(value)
      case "Compare" =>
        val left: Expr = applyToJsField(ast, "left", parseJson(AsExpr))
        val ops: List[CompOp] = applyToJsList(ast, "ops", parseJson(AsCompOp)) 
        val rights: List[Expr] = applyToJsList(ast, "comparators", parseJson(AsExpr))
        CompExpr(left, ops zip rights)
      case "Call" =>
        val func: Expr = applyToJsField(ast, "func", parseJson(AsExpr))
        val args: List[Expr] = applyToJsList(ast, "args", parseJson(AsExpr))
        val kwds: List[Keyword] = applyToJsList(ast, "keywords", parseJson(AsKeyword))
        Call(func, args, kwds)
      case "FormattedValue" => ??? // TODO impl
      case "JoinedStr" =>
        val vs: List[Expr] = applyToJsList(ast, "values", parseJson(AsExpr))
        JoinedStr(vs)
      case "Constant" =>
        val const: Const = applyToJsField(ast, "value", parseJson(AsConst))
        EConst(const) 
      case "Attribute" =>
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr))
        val attr: Id = applyToJsField(ast, "attr", parseJson(AsId))
        Attribute(value, attr)
      case "Subscript" =>
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr))
        val slice: Expr = applyToJsField(ast, "slice", parseJson(AsExpr))
        Subscript(value, slice) 
      case "Starred" =>
        val value: Expr = applyToJsField(ast, "value", parseJson(AsExpr))
        Starred(value)
      case "Name" =>
        val name: Id = applyToJsField(ast, "value", parseJson(AsId))
        EName(name)
      case "List" =>
        val elts: List[Expr] = applyToJsList(ast, "elts", parseJson(AsExpr))
        ListExpr(elts) 
      case "Tuple" =>
        val elts: List[Expr] = applyToJsList(ast, "elts", parseJson(AsExpr))
        TupleExpr(elts)
      case "Slice" =>
        val lower: Option[Expr] = applyToJsOption(ast, "lower", parseJson(AsExpr)) 
        val upper: Option[Expr] = applyToJsOption(ast, "upper", parseJson(AsExpr))
        val step: Option[Expr] = applyToJsOption(ast, "step", parseJson(AsExpr))
        Slice(lower, upper, step)
    }

  // comprehension
  def parseComprehensionJson(ast: JsObject): Comprehension =
    getJsObjectType(ast) match {
      case "comprehension" =>
        val target: Expr = applyToJsField(ast, "target", parseJson(AsExpr))
        val iter: Expr = applyToJsField(ast, "iter", parseJson(AsExpr))
        val ifs: List[Expr] = applyToJsList(ast, "ifs", parseJson(AsExpr))
        val isAsync: Boolean = ast.fields("is_async") match {
          case JsNumber(n) => if (n == 1) true else false
        }
        if (isAsync) {
          Compre(target, iter, ifs)
        } else {
          AsyncCompre(target, iter, ifs)
        }
    }

  // parsing op
  def parseBoolOpJson(ast: JsObject): BoolOp =
    getJsObjectType(ast) match {
      case "And" => OAnd
      case "Or" => OOr
    }

  def parseBinOpJson(ast: JsObject): BinOp = 
    getJsObjectType(ast) match {
      case "Add" => OAdd
      case "Sub" => OSub
      case "Mult" => OMul 
      case "MatMult" => ??? // TODO what is this op? 
      case "Div" => ODiv
      case "Mod" => OMod
      case "Pow" => OPow
      case "LShift" => OLShift 
      case "RShift" => ORShift
      case "BitOr" => OBOr
      case "BitXor" => OBXor 
      case "BitAnd" => OBAnd
      case "FloorDiv" => OIDiv 
    }

  def parseUnaryOpJson(ast: JsObject): UnOp =
    getJsObjectType(ast) match {
      case "Invert" => UInv
      case "Not" => UNot
      case "UAdd" => UPlus 
      case "USub" => UMinus
    }

  def parseCompOpJson(ast: JsObject): CompOp = 
    getJsObjectType(ast) match {
      case "Eq" => CEq
      case "NotEq" => CNeq 
      case "Lt" => CLt
      case "LtE" => CLte  
      case "Gt" => CGt
      case "GtE" => CGte 
      case "Is" => CIs
      case "IsNot" => CIsNot 
      case "In" => CIn
      case "NotIn" => CNotIn  
    }

  // Execption handler
  def parseExcHandlerJson(ast: JsObject): ExcHandler =
    getJsObjectType(ast) match {
      case "ExceptHandler" =>
        val target: Option[Expr] = ast.fields("type") match {
          case JsNull => None
          case x => Some(parseExprJson(x.asJsObject))
        }
        val asname: Option[Id] = ast.fields("name") match {
          case JsNull => None
          case x => Some(parseIdJson(x.asJsObject))
        }
        val body: List[Stmt] = ast.fields("body") match {
          case JsArray(l) => l.map(v => parseStmtJson(v.asJsObject)).toList
        }
        ExcHandler(target, asname, body)
    }

  // Call expression arguments
  def parseArgsJson(ast: JsObject): Args =
    getJsObjectType(ast) match {
      case "arguments" =>
        val posonlyArgs: List[Arg] = ast.fields("posonlyargs") match {
          case JsArray(l) => l.map(v => parseArgJson(v.asJsObject)).toList
        }
        val args: List[Arg] = ast.fields("args") match {
          case JsArray(l) => l.map(v => parseArgJson(v.asJsObject)).toList
        }
        val vararg: Option[Arg] = ast.fields("vararg") match {
          case JsNull => None
          case v => Some(parseArgJson(v.asJsObject)) 
        }
        val kwonlyargs: List[Arg] = ast.fields("kwonlyargs") match {
          case JsArray(l) => l.map(v => parseArgJson(v.asJsObject)).toList
        }
        val kwDefaults: List[Option[Expr]] = ast.fields("kw_defaults") match {
          case JsArray(l) => l.map(v => v match {
            case JsNull => None
            case _ => Some(parseExprJson(v.asJsObject))
          }).toList
        }
        val kwarg: Option[Arg] = ast.fields("kwarg") match {
          case JsNull => None
          case v => Some(parseArgJson(v.asJsObject))  
        }
        var defaults: List[Option[Expr]] = ast.fields("defaults") match {
          case JsArray(l) => l.map(v => Some(parseExprJson(v.asJsObject))).toList
        }
        val posonlys = posonlyArgs zip defaults.map(v => Some(v))

        // adjusting defaults list for length of args + posonlyArgs
        val totalPosArgs = posonlyArgs.length + args.length
        val givenPosDefaults = defaults.length
        if (givenPosDefaults < totalPosArgs) {
          defaults = List.fill(totalPosArgs - givenPosDefaults)(None) ++ defaults 
        }
        val (posonlyDefaults, normDefaults) = defaults.splitAt(posonlyArgs.length)
        val posOnlys = posonlyArgs zip posonlyDefaults
        val normArgs = args zip normDefaults
        val keyOnlys = kwonlyargs zip kwDefaults 
        Args(posOnlys, normArgs, vararg, keyOnlys, kwarg)
    }

  def parseArgJson(ast: JsObject): Arg = ??? 

  def parseKeywordJson(ast: JsObject): Keyword = 
    getJsObjectType(ast) match {
      case "keyword" =>
        val argExpr = parseExprJson(ast.fields("value").asJsObject)
        // case by `arg` field, DoubleStar or just kwarg
        ast.fields("arg") match {
          case JsNull => Keyword(None, argExpr)
          case JsString(s) => Keyword(Some(Id(s)), argExpr) 
        }
    }

  // Alias
  def parseAliasJson(ast: JsObject): Alias = 
    getJsObjectType(ast) match {
      case "alias" =>
        val nameRaw: String = extractJsStr(ast.fields("name"))
        val name: List[Id] = nameRaw.split(".").map(s => Id(s)).toList
        // case by value of asname
        ast.fields("asname") match {
          case JsNull => Alias(name, None)  
          case JsString(s) => Alias(name, Some(Id(s)))
        }
    }

  // WithItem
  def parseWithItemJson(ast: JsObject): WithItem = 
    getJsObjectType(ast) match {
      case "withitem" => 
        val ctxExpr: Expr = parseExprJson(ast.fields("context_expr").asJsObject)
        // case by value of optional_vars
        ast.fields("optional_vars") match {
          case JsNull => WithItem(ctxExpr, None)
          case _ =>
            val asExpr: Expr = parseExprJson(ast.fields("optional_vars").asJsObject)
            WithItem(ctxExpr, Some(asExpr))
        }
    }

  // type ignore
  def parseTypeIgnoreJson(ast: JsObject) = ???

  // parser for identifier
  def parseIdJson(ast: JsObject): Id =
    getJsObjectType(ast) match {
      case "Name" =>
        val name = extractJsStr(ast.fields("name"))
        Id(name)
    }

  // parser for constnat
  def parseConstJson(ast: JsObject): Const = 
    getJsObjectType(ast) match {
      case "Constant" => 
        ast.fields("value") match {
          case x: JsBoolean => BooleanLiteral(x.value)
          case JsNull => NoneLiteral
          // TODO complex numbers are represented as string. ignore now
          case JsString(s) => StringLiteral(s)
          // TODO how to get int
          case JsNumber(n) => FloatLiteral(n.doubleValue)
        }
    }

  // json helpers
  def getJsObjectType(ast: JsObject): String =
    extractJsStr(ast.fields("_type"))

  def extractJsStr(ast: JsValue): String = ast match {
    case JsString(s) => s
    case _ => throw new RuntimeException("Not JsString")
  }

  def extractJsInt(ast: JsValue): Int = ast match {
    case JsNumber(n) if n.isValidInt => n.intValue  
  }

  def extractJsReal(ast: JsValue): Double = ast match {
    case JsNumber(n) if n.isDecimalDouble => n.doubleValue
  }

  def applyToJsList[T](ast: JsObject, fieldname: String, f: JsValue => T): List[T] =
    ast.fields(fieldname) match {
      case JsArray(l) => l.map(f(_)).toList
    }
  def applyToJsField[T](ast: JsObject, fieldname: String, f: JsObject => T): T =
    f(ast.fields(fieldname).asJsObject)
  def applyToJsOption[T](ast: JsObject, fieldname: String, f: JsObject => T): Option[T] =
    ast.fields(fieldname) match {
      case JsNull => None
      case x => Some(f(x.asJsObject))
    }
}

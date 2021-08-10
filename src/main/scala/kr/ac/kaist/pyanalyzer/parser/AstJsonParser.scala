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
    val astJson = sourceToJson(source)
    val ast = parseAstJson(astJson)
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
  // TODO wrong
  def parseAstJson(ast: JsObject): Node = ast.fields.get("_type") match {
    case Some(jsVal) => jsVal.toString match {
      case "\"Module\"" => parseModuleJson(ast)
      case _ => println(jsVal.toString); ???
    }
    case None => throw new RuntimeException("No `_type` field found")
  }

  // parses top-level (Module) ast-json into Module case class
  def parseModuleJson(ast: JsObject): Module = 
    getJsObjectType(ast) match {
      case "Module" =>
        val body: List[JsValue] = ast.fields.get("body") match {
          case Some(JsArray(l)) => l.toList
        }
        Module(body.map(v => parseStmtJson(v.asJsObject)).toList)
    }

  // parses stmt ast-json
  def parseStmtJson(ast: JsObject): Stmt = 
    getJsObjectType(ast) match {
      case "FunctionDef" =>
        val funcName = ast.fields("name").toString
        val argsObj = ast.fields("args").asJsObject
        val args = parseArgsJson(argsObj)
        val body = ast.fields("body") match {
          case JsArray(l) => l.map(v => parseStmtJson(v.asJsObject)).toList
        }
        FunDef(Nil, Id(funcName), args, None, None, body) 
      case "AsyncFunctionDef" => ???  
      case "ClassDef" => ???  
      case "Return" => ???  
      case "Delete" => ???  
      case "Assign" => ???  
      case "AugAssign" => ???  
      case "AnnAssign" => ???  
      case "For" => ???  
      case "AsyncFor" => ???  
      case "While" => ???  
      case "If" => ???  
      case "With" => ???  
      case "AsyncWith" => ???  
      case "Raise" => ???  
      case "Try" => ???  
      case "Assert" => ???  
      case "Import" => ???  
      case "ImportFrom" => ???  
      case "Global" => ???  
      case "Nonlocal" => ???  
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
      case "BoolOp" => ???
      case "NamedExpr" => ???
      case "BinOp" => ???
      case "UnaryOp" => ???
      case "Lambda" => ???
      case "IfExp" => ???
      case "Dict" => ???
      case "Set" => ???
      case "ListComp" => ???
      case "SetComp" => ???
      case "DictComp" => ???
      case "GeneratorExp" => ???
      case "Await" => ???
      case "Yield" => ???
      case "YieldFrom" => ???
      case "Compare" => ???
      case "Call" => ???
      case "FormattedValue" => ???
      case "JoinedStr" => ???
      case "Constant" => ???
      case "Attribute" => ???
      case "Subscript" => ???
      case "Starred" => ???
      case "Name" => ???
      case "List" => ???
      case "Tuple" => ???
      case "Slice" => ???
    }

  // parsing op
  def parseBoolOpJson(ast: JsObject): Op =
    getJsObjectType(ast) match {
      case "And" => ???
      case "Or" => ???
    }

  def parseOperatorJson(ast: JsObject): Op = 
    getJsObjectType(ast) match {
      case "Add" => ??? 
      case "Sub" => ??? 
      case "Mult" => ??? 
      case "MatMult" => ??? 
      case "Div" => ??? 
      case "Mod" => ??? 
      case "Pow" => ??? 
      case "LShift" => ??? 
      case "RShift" => ??? 
      case "BitOr" => ??? 
      case "BitXor" => ??? 
      case "BitAnd" => ??? 
      case "FloorDiv" => ??? 
    }

  def parseUnaryOpJson(ast: JsObject): Op =
    getJsObjectType(ast) match {
      case "Invert" => ??? 
      case "Not" => ??? 
      case "UAdd" => ??? 
      case "USub" => ??? 
    }

  def parseCmpOpJson(ast: JsObject): Op = 
    getJsObjectType(ast) match {
      case "Eq" => ???  
      case "NotEq" => ???  
      case "Lt" => ???  
      case "LtE" => ???  
      case "Gt" => ???  
      case "GtE" => ???  
      case "Is" => ???  
      case "IsNot" => ???  
      case "In" => ???  
      case "NotIn" => ???  
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

}

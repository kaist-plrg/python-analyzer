package kr.ar.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer._
import org.scalatest.funsuite._
import kr.ac.kaist.pyanalyzer.parser._
import kr.ac.kaist.pyanalyzer.parser.ast._
import kr.ac.kaist.pyanalyzer.parser.ast.Beautifier._
import kr.ac.kaist.pyanalyzer.util.Appender._
import kr.ac.kaist.pyanalyzer.util.Useful._
import kr.ac.kaist.pyanalyzer.parser.TokenListParser._
import scala.Console._ 

// FileParseTest: iterate over Python source files, test to parse and unparse correctly
// files should locate in $PY_SOURCE_DIR, and subdirectories must be category and name
// and the filename should be `main.py` (refer to makeFileName)

class FileParseTest extends AnyFunSuite {
  val help = s"""Test parsing and unparsing Python source files"""
  var epoch = 1
  // `setPrompt = true` to see message
  var setPrompt = true
  def prompt(s: String): Unit = if (setPrompt) { println(s) }

  // locating python source file
  def makeFileName(cat: String, name: String): String = s"$PY_SOURCE_DIR/$cat/$name/main.py"

  // parsing routine
  def parseSource(t: String) = {
    val tokens = SourceParser.tokenizeText(t)
    prompt(s"${CYAN}tokenized result:${RESET}\n${Token.coloredTokens(tokens)}")
    val reader = new PackratReader(TokenReader(tokens))
    val parser = TokenListParser.statements
    parser(reader) match {
      case Success(result, rest) => result
      case result => throw new RuntimeException(s"Parsing fail\ntest:\n\n$result")
    }
  }
  
  // main testing routine
  def testFile(cat: String, name: String): Unit = test(s"FileParseTest [$cat:$name]"){
    val filename = makeFileName(cat, name)
    try { 
      prompt("================================")
      prompt(s"${MAGENTA}Test count: $epoch\n${RESET}")

      val text = SourceParser.readSource(filename)
      prompt(s"${CYAN}Source Text:${RESET}\n$text\n")

      prompt("----First Parse--------------------")
      val ast01 = parseSource(text) 
      prompt(s"${CYAN}First ast:${RESET}\n$ast01\n")
      
      val pretty01: String = beautify(ast01)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty01\n")

      prompt("----Second Parse--------------------")
      val ast02 = parseSource(pretty01)
      prompt(s"${CYAN}Second ast:${RESET}\n$ast02\n")

      val pretty02: String = beautify(ast02)
      prompt(s"${CYAN}Beautified as:${RESET}\n$pretty02\n")
      
      assert(pretty01 == pretty02)
      prompt("================================")
    } catch { 
      case e => 
        throw e
        println(s"${MAGENTA}Epoch $epoch failed:${RESET}\n$e\n")
        fail
    }
    finally {
      epoch += 1
    }
  }

  // test targets : list of (category_name, test_name list)
  // refer to src/main/resources/py-source
  val targets: List[(String, List[String])] = List(
    ("simple", List(
      "test01", 
      "parser-regress"
    )),
    ("assignments", List(
      "chained",
      "recursive_tuple",
      "starred",
      "tuple",
    )),
    ("builtins", List(
      "functions",
      "map",
      "types",
    )),
    ("classes", List(
      "assigned_call",
      "assigned_self_call",
      "base_class_attr",
      "base_class_calls_child",
      "call",
      "direct_call",
      "imported_attr_access",
      "imported_call",
      "imported_call_without_init",
      "imported_nested_attr_access",
      "instance",
      "nested_call",
      "nested_class_calls",
      "parameter_call",
      "return_call",
      "return_call_direct",
      "self_assign_func",
      "self_assignment",
      "self_call",
      "static_method_call",
      "super_class_return",
      "tuple_assignment",
    )),
    ("decorators", List(
      "assigned",
      "call",
      "nested",
      "nested_decorators",
      "param_call",
      "return",
      "return_different_func",
    )),
    ("dicts", List(
      "add_key",
      "assign",
      "call",
      "ext_key",
      "nested",
      "new_key_param",
      "param",
      "param_key",
      "return",
      "return_assign",
      "type_coercion",
      "update",
    )),
    ("direct_calls", List(
      "assigned_call",
      "imported_return_call",
      "return_call",
      "with_parameters",
    )),
    ("exceptions", List(
      "raise",
      "raise_assigned",
      "raise_attr",
    )),
    ("functions", List(
      "assigned_call",
      "assigned_call_lit_param",
      "call",
      "imported_call",
    )),
    ("generators", List(
      "iter_param",
      "iter_return",
      "iterable",
      "iterable_assigned",
      "no_iter",
      "yield",
    )),
    ("imports", List(
      "chained_import",
      "import_all",
      "import_as",
      "import_from",
      "init_func_import",
      "init_import",
      "parent_import",
      "relative_import",
      "relative_import_with_name",
      "simple_import",
      "submodule_import",
      "submodule_import_all",
      "submodule_import_as",
      "submodule_import_from",
    )),
    ("kwargs", List(
      "assigned_call",
      "call",
      "chained_call",
    )),
    ("lambdas", List(
      "call",
      "calls_parameter",
      "chained_calls",
      "parameter_call",
      "return_call",
    )),
    ("lists", List(
      "simple", 
      "slice",
      "param_index",
      "nested",
      "nested_comprehension",
      "ext_index",
      "comprehension_if",
      "comprehension_val",
    )),
    ("mro", List(
      "basic",
      "basic_init",
      "parents_same_superclass",
      "self_assignment",
      "super_call",
      "two_parents",
      "two_parents_method_defined",
    )),
    ("parameters", List(
      "assigned_call",
      "call",
      "imported_assigned_call",
      "imported_call",
      "nested_call",
      "param_call",
    )),
    ("returns", List(
      "call",
      "imported_call",
      "nested_import_call",
      "return_complex",
    )),
  )

  def init: Unit = {
    prompt(help)
    
    for {
      (cat, names) <- targets
      name <- names
    } testFile(cat, name)
  }

  init
}

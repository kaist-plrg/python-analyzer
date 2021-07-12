package kr.ac.kaist.pyanalyzer.parser

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.parser._

// Microbenchmark from PyCG
// set of small Python source files
object Microbenchmark extends Microbenchmarks
class Microbenchmarks extends FileTestSet {
  def filePaths: Iterator[(String, String)] =
    for {
      (cat, names) <- targets.iterator
      name <- names.iterator
    } yield (s"microbenchmarks:$cat:$name", makeFilePath(cat, name))

  val rootPath: String = s"$RESOURCE_DIR/microbenchmarks"
  
  // test targets : list of (category_name, test_name list)
  def makeFilePath(cat: String, name: String): String = s"$rootPath/$cat/$name/main.py" 
  val targets: List[(String, List[String])] = List(
    ("simple", List(
      "test01", 
      //"parser-regress"
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
}

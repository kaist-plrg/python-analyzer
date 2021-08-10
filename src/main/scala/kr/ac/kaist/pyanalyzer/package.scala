package kr.ac.kaist

package object pyanalyzer {
  // const directory strings
  val BASE_DIR = System.getenv("PYANALYZER_HOME")

  val SRC_DIR = s"$BASE_DIR/src"
  val RESOURCE_DIR = s"$SRC_DIR/main/resources"

  val HOROVOD_DIR = s"$BASE_DIR/tensorflow-models/tensorflow-to-horovod"

  val LOG_DIR = s"$BASE_DIR/logs"
  val TEST_LOG_DIR = s"$LOG_DIR/test"
  
  val PY_AST_DIR = s"$BASE_DIR/py-ast"

  // Others
  val LINE_SEP = System.getProperty("line.separator")
}

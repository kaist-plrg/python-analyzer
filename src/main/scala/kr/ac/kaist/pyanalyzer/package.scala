package kr.ac.kaist

package object pyanalyzer {
  val BASE_DIR = System.getenv("PYANALYZER_HOME")

  val SRC_DIR = s"$BASE_DIR/src"
  val RESOURCE_DIR = s"$SRC_DIR/main/resources"

  val LINE_SEP = System.getProperty("line.separator")
}

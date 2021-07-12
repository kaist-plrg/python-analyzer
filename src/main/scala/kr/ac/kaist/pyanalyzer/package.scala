package kr.ac.kaist

package object pyanalyzer {
  // const directory strings
  val BASE_DIR = System.getenv("PYANALYZER_HOME")

  val SRC_DIR = s"$BASE_DIR/src"
  val RESOURCE_DIR = s"$SRC_DIR/main/resources"

  val HOROVOD_DIR = s"$BASE_DIR/tensorflow-models/tensorflow-to-horovod"

  val LINE_SEP = System.getProperty("line.separator")
}

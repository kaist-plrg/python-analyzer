package kr.ac.kaist

package object pyanalyzer {
  // const directory strings
  val BASE_DIR = System.getenv("PYANALYZER_HOME")

  val SRC_DIR = s"$BASE_DIR/src"
  val RESOURCE_DIR = s"$SRC_DIR/main/resources"

  val HOROVOD_DIR = s"$BASE_DIR/tensorflow-models/tensorflow-to-horovod"

  val LOG_DIR = s"$BASE_DIR/logs"
  val TEST_LOG_DIR = s"$LOG_DIR/test"
  val TRANS_LOG_DIR = s"$LOG_DIR/transform"
  
  val PY_AST_DIR = s"$BASE_DIR/py-ast"

  val OUTPUT_DIR = s"$BASE_DIR/output"

  val TEST_DIR = s"$BASE_DIR/test"

  // tutorial
  val TUTORIAL_DIR = s"$BASE_DIR/TensorFlow-2.x-Tutorials"

  // tutorial
  val TUTORIAL_DIR_2 =
    s"$BASE_DIR/TensorFlow-Examples/examples/3_NeuralNetworks"

  // cnn
  val CNN_DIR = s"$BASE_DIR/cnn-text-classification-tf"

  // Others
  val LINE_SEP = System.getProperty("line.separator")
}

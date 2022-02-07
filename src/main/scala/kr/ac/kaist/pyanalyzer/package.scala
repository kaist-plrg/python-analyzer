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
  val tutoModelPattern = "\\d\\d.*".r
  // tutorial test
  val MAINSCRIPTS_MAP = Map(
    "01-TF2.0-Overview" -> List(
      "conv_train.py",
      "fc_train.py"
    ),
    "05-FashionMNIST" -> List(
      "mnist_Seqential_gradient.py",
      "mnist_fit.py",
      "mnist_custommodel.py",
      "mnist_matmul.py",
    ),
    "21-CN-EN-Translation-BERT" -> List(
      "bert_train.py",
      "transformer_train.py",
    ),
  )
  def mainscriptOf(model: String): List[String] =
    MAINSCRIPTS_MAP.getOrElse(model, List(""))

  // Others
  val LINE_SEP = System.getProperty("line.separator")
}

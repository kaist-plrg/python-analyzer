package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.transformer.ClassOrder._
import kr.ac.kaist.pyanalyzer.util.Useful._

package object transformer {
  val OPTIMIZER = List(
    // tf2
    "tensorflow.optimizers.Adadelta",
    "tensorflow.optimizers.Adagrad",
    "tensorflow.optimizers.Adam",
    "tensorflow.optimizers.Adamax",
    "tensorflow.optimizers.Ftrl",
    "tensorflow.optimizers.Nadam",
    "tensorflow.optimizers.RMSprop",
    "tensorflow.optimizers.SGD",
    "tensorflow.keras.optimizers.Adadelta",
    "tensorflow.keras.optimizers.Adagrad",
    "tensorflow.keras.optimizers.Adam",
    "tensorflow.keras.optimizers.Adamax",
    "tensorflow.keras.optimizers.Ftrl",
    "tensorflow.keras.optimizers.Nadam",
    "tensorflow.keras.optimizers.RMSprop",
    "tensorflow.keras.optimizers.SGD",
  )
  val MODEL = List(
    "tensorflow.keras.Model",
    "tensorflow.keras.models.Model",
  )
  private val MODEL_SUBCLASS_RELATION = List(
    ("tensorflow.keras.models.Sequential", "tensorflow.keras.Model"),
    ("tensorflow.keras.models.Sequential", "tensorflow.keras.models.Model"),
    ("tensorflow.keras.models.Functional", "tensorflow.keras.Model"),
    ("tensorflow.keras.models.Functional", "tensorflow.keras.models.Model"),
  )
  val LEARNING_RATE_SCHEDULER = List(
    "tensorflow.keras.optimizers.schedules.CosineDecay",
    "tensorflow.keras.optimizers.schedules.CosineDecayRestarts",
    "tensorflow.keras.optimizers.schedules.ExponentialDecay",
    "tensorflow.keras.optimizers.schedules.InverseTimeDecay",
    "tensorflow.keras.optimizers.schedules.PolynomialDecay",
    "tensorflow.keras.optimizers.schedules.PiecewiseConstantDecay",
    "tensorflow.keras.experimental.CosineDecay",
    "tensorflow.keras.experimental.CosineDecayRestarts",
  )
  val CONST_LEARNING_RATE_SCHEDULER = List(
    "tensorflow.keras.optimizers.schedules.PiecewiseConstantDecay",
    "tensorflow.keras.experimental.PiecewiseConstantDecay",
  )
  val WRITE_METHOD = List(
    "write", "summary", "save_weights", "load_weights", "save"
  )

  val GIVEN_CLASS_ORDER = ClassOrder()
    .addNode( // list of nodes
      (OPTIMIZER ++ MODEL).map(parseStrFullname(_))
    ).addEdge( // list of subclass pairs (child, parent)
      MODEL_SUBCLASS_RELATION.map(p => (parseStrFullname(p._1), parseStrFullname(p._2)))
    )
  val TRANS_PRINT_WRITER = getPrintWriter(s"$TRANS_LOG_DIR/Warnings")
}

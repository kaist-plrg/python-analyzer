package kr.ac.kaist.pyanalyzer

import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder
import kr.ac.kaist.pyanalyzer.hierarchy.ClassOrder._
import kr.ac.kaist.pyanalyzer.util.Useful._

package object transformer {
  val OPTIMIZER_TF_V2 = List(
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
  val OPTIMIZER_TF_V1 = List(
    "tensorflow.compat.v1.train.AdadeltaOptimizer",
    "tensorflow.compat.v1.train.AdagradDAOptimizer",
    "tensorflow.compat.v1.train.AdagradOptimizer",
    "tensorflow.compat.v1.train.AdamOptimizer",
    "tensorflow.compat.v1.train.GradientDescentOptimizer",
    "tensorflow.compat.v1.train.MomentumOptimizer",
    "tensorflow.compat.v1.train.RMSPropOptimizer",
  )
  val OPTIMIZER = OPTIMIZER_TF_V1 ++ OPTIMIZER_TF_V2
  val MODEL = List(
    "tensorflow.keras.Model",
    "tensorflow.keras.models.Model",
  )
  val MODEL_SUBCLASS = List(
    "tensorflow.keras.models.Sequential",
    "tensorflow.keras.models.Functional",
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
  val LEARNING_RATE_SCHEDULER_TF_V1 = List(
    "tensorflow.compat.v1.train.exponential_decay"
  )
  val CONST_LEARNING_RATE_SCHEDULER = List(
    "tensorflow.keras.optimizers.schedules.PiecewiseConstantDecay",
    "tensorflow.keras.experimental.PiecewiseConstantDecay",
  )
  val WRITE_METHOD = List(
    "write", "summary", "save_weights", "load_weights", "save"
  )

  private val NODES = OPTIMIZER ++ OPTIMIZER_TF_V1 ++ MODEL ++
      LEARNING_RATE_SCHEDULER ++ LEARNING_RATE_SCHEDULER_TF_V1
  private val MODEL_SUBCLASS_RELATION =
    for {
      model <- MODEL
      modelSubclass <- MODEL_SUBCLASS
    } yield (modelSubclass, model)
  val GIVEN_CLASS_ORDER = ClassOrder()
    .addNode( // list of nodes
      NODES.map(parseStrFullname(_))
    ).addEdge( // list of subclass pairs (child, parent)
      MODEL_SUBCLASS_RELATION.map(
        p => (parseStrFullname(p._1), parseStrFullname(p._2))
      )
    )
  val TRANS_PRINT_WRITER = getPrintWriter(s"$TRANS_LOG_DIR/Warnings")
}

import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
from tensorflow.keras import datasets, layers, optimizers, Sequential, metrics
import datetime
current_time = datetime.datetime.now().strftime("%Y%m%d-%H%M%S", )
train_log_dir = "logs/org-board/" + current_time + "/train"
train_summary_writer = tf.summary.create_file_writer(train_log_dir, )
((xs, ys), _) = datasets.mnist.load_data()
if hvd.rank() == 0:
  print("datasets:", xs.shape, ys.shape, xs.min(), xs.max(), )
xs = tf.convert_to_tensor(xs, dtype=tf.float32, ) / 255.0
db = tf.data.Dataset.from_tensor_slices((xs, ys), )
db = db.batch(32, ).repeat(10, )
network = Sequential([layers.Dense(256, activation="relu", ), layers.Dense(256, activation="relu", ), layers.Dense(256, activation="relu", ), layers.Dense(10, )], )
network.build(input_shape=(None, 28 * 28), )
if hvd.rank() == 0:
  network.summary()
optimizer = optimizers.SGD(learning_rate=0.01 * hvd.size(), )
acc_meter = metrics.Accuracy()
for (step, (x, y)) in enumerate(db, ):
  with tf.GradientTape() as tape:
    x = tf.reshape(x, (-1, 28 * 28), )
    out = network(x, )
    y_onehot = tf.one_hot(y, depth=10, )
    loss = tf.square(out - y_onehot, )
    loss = tf.reduce_sum(loss, ) / 32
  tape = hvd.DistributedGradientTape(tape, )
  with train_summary_writer.as_default():
    tf.summary.scalar("loss", loss, step=step, )
  acc_meter.update_state(tf.argmax(out, axis=1, ), y, )
  grads = tape.gradient(loss, network.trainable_variables, )
  optimizer.apply_gradients(zip(grads, network.trainable_variables, ), )
  if not hvd_broadcast_done:
    hvd.broadcast_variables(network.variables, root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
  if step % 200 == 0:
    if hvd.rank() == 0:
      print(step, "loss:", float(loss, ), "acc:", acc_meter.result().numpy(), )
    acc_meter.reset_states()

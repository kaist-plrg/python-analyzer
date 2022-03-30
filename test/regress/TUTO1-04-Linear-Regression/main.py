import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
import numpy as np
from tensorflow import keras
import os
import datetime
current_time = datetime.datetime.now().strftime("%Y%m%d-%H%M%S", )
train_log_dir = "logs/org-board/" + current_time + "/train"
train_summary_writer = tf.summary.create_file_writer(train_log_dir, )
class Regressor(keras.layers.Layer, ):
  def __init__(self, ):
    super(Regressor, self, ).__init__()
    self.w = self.add_variable("meanless-name", [13, 1], )
    self.b = self.add_variable("meanless-name", [1], )
    if hvd.rank() == 0:
      print(self.w.shape, self.b.shape, )
    if hvd.rank() == 0:
      print(type(self.w, ), tf.is_tensor(self.w, ), self.w.name, )
    if hvd.rank() == 0:
      print(type(self.b, ), tf.is_tensor(self.b, ), self.b.name, )
  def call(self, x, ):
    x = tf.matmul(x, self.w, ) + self.b
    return x
def main():
  tf.random.set_seed(22, )
  np.random.seed(22, )
  os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
  assert tf.__version__.startswith("2.", )
  ((x_train, y_train), (x_val, y_val)) = keras.datasets.boston_housing.load_data()
  (x_train, x_val) = (x_train.astype(np.float32, ), x_val.astype(np.float32, ))
  if hvd.rank() == 0:
    print(x_train.shape, y_train.shape, x_val.shape, y_val.shape, )
  db_train = tf.data.Dataset.from_tensor_slices((x_train, y_train), ).batch(64, )
  db_val = tf.data.Dataset.from_tensor_slices((x_val, y_val), ).batch(102, )
  model = Regressor()
  criteon = keras.losses.MeanSquaredError()
  optimizer = keras.optimizers.Adam(learning_rate=0.01 * hvd.size(), )
  for epoch in range(200, ):
    for (step, (x, y)) in enumerate(db_train, ):
      with tf.GradientTape() as tape:
        logits = model(x, )
        logits = tf.squeeze(logits, axis=1, )
        loss = criteon(y, logits, )
      tape = hvd.DistributedGradientTape(tape, )
      grads = tape.gradient(loss, model.trainable_variables, )
      optimizer.apply_gradients(zip(grads, model.trainable_variables, ), )
      global hvd_broadcast_done
      if not hvd_broadcast_done:
        hvd.broadcast_variables(model.variables, root_rank=0, )
        hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
        hvd_broadcast_done = True
      with train_summary_writer.as_default():
        tf.summary.scalar("loss", loss, step=200 * epoch + step, )
    if hvd.rank() == 0:
      print(epoch, "loss:", loss.numpy(), )
    if epoch % 10 == 0:
      for (x, y) in db_val:
        logits = model(x, )
        logits = tf.squeeze(logits, axis=1, )
        loss = criteon(y, logits, )
        if hvd.rank() == 0:
          print(epoch, "val loss:", loss.numpy(), )
if __name__ == "__main__":
  main()

import os
import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
from tensorflow import keras
from tensorflow.keras import datasets, layers, optimizers
import argparse
import numpy as np
import datetime
current_time = datetime.datetime.now().strftime("%Y%m%d-%H%M%S", )
train_log_dir = "logs/org-board-epoch/" + current_time + "/train"
train_summary_writer = tf.summary.create_file_writer(train_log_dir, )
from network import VGG16
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
argparser = argparse.ArgumentParser()
argparser.add_argument("--train_dir", type=str, default="/tmp/cifar10_train", help="Directory where to write event logs and checkpoint.", )
argparser.add_argument("--max_steps", type=int, default=1000000, help="Number of batches to run.", )
argparser.add_argument("--log_device_placement", action="store_true", help="Whether to log device placement.", )
argparser.add_argument("--log_frequency", type=int, default=10, help="How often to log results to the console.", )
def normalize(X_train, X_test, ):
  X_train = X_train / 255.0
  X_test = X_test / 255.0
  mean = np.mean(X_train, axis=(0, 1, 2, 3), )
  std = np.std(X_train, axis=(0, 1, 2, 3), )
  if hvd.rank() == 0:
    print("mean:", mean, "std:", std, )
  X_train = (X_train - mean) / (std + 1.0E-7)
  X_test = (X_test - mean) / (std + 1.0E-7)
  return (X_train, X_test)
def prepare_cifar(x, y, ):
  x = tf.cast(x, tf.float32, )
  y = tf.cast(y, tf.int32, )
  return (x, y)
def compute_loss(logits, labels, ):
  return tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=labels, ), )
def main():
  tf.random.set_seed(22, )
  if hvd.rank() == 0:
    print("loading data...", )
  ((x, y), (x_test, y_test)) = datasets.cifar10.load_data()
  (x, x_test) = normalize(x, x_test, )
  if hvd.rank() == 0:
    print(x.shape, y.shape, x_test.shape, y_test.shape, )
  train_loader = tf.data.Dataset.from_tensor_slices((x, y), )
  train_loader = train_loader.map(prepare_cifar, ).shuffle(50000, ).batch(256, )
  test_loader = tf.data.Dataset.from_tensor_slices((x_test, y_test), )
  test_loader = test_loader.map(prepare_cifar, ).shuffle(10000, ).batch(256, )
  if hvd.rank() == 0:
    print("done.", )
  model = VGG16([32, 32, 3], )
  criteon = keras.losses.CategoricalCrossentropy(from_logits=True, )
  metric = keras.metrics.CategoricalAccuracy()
  optimizer = optimizers.Adam(learning_rate=1.0E-4 * hvd.size(), )
  for epoch in range(250, ):
    for (step, (x, y)) in enumerate(train_loader, ):
      y = tf.squeeze(y, axis=1, )
      y = tf.one_hot(y, depth=10, )
      with tf.GradientTape() as tape:
        logits = model(x, )
        loss = criteon(y, logits, )
        metric.update_state(y, logits, )
      tape = hvd.DistributedGradientTape(tape, )
      grads = tape.gradient(loss, model.trainable_variables, )
      grads = [tf.clip_by_norm(g, 15, ) for g in grads]
      optimizer.apply_gradients(zip(grads, model.trainable_variables, ), )
      global hvd_broadcast_done
      if not hvd_broadcast_done:
        hvd.broadcast_variables(model.variables, root_rank=0, )
        hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
        hvd_broadcast_done = True
      if step % 40 == 0:
        if hvd.rank() == 0:
          print(epoch, step, "loss:", float(loss, ), "acc:", metric.result().numpy(), )
        metric.reset_states()
    if epoch % 1 == 0:
      metric = keras.metrics.CategoricalAccuracy()
      for (x, y) in test_loader:
        y = tf.squeeze(y, axis=1, )
        y = tf.one_hot(y, depth=10, )
        logits = model.predict(x, )
        metric.update_state(y, logits, )
      if hvd.rank() == 0:
        print("test acc:", metric.result().numpy(), )
      metric.reset_states()
    with train_summary_writer.as_default():
      tf.summary.scalar("loss", loss, step=epoch, )
if __name__ == "__main__":
  main()

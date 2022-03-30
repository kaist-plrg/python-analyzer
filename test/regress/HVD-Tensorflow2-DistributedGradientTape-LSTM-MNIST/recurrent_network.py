from __future__ import absolute_import, division, print_function
import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
from tensorflow.keras import Model, layers
import numpy as np
from tensorflow.keras.datasets import mnist
num_classes = 10
num_features = 784
learning_rate = 0.001
training_steps = 10000
batch_size = 32
display_step = 100
num_input = 28
timesteps = 28
num_units = 32
((x_train, y_train), (x_test, y_test)) = mnist.load_data()
(x_train, x_test) = (np.array(x_train, np.float32, ), np.array(x_test, np.float32, ))
(x_train, x_test) = (x_train.reshape([-1, 28, 28], ), x_test.reshape([-1, num_features], ))
(x_train, x_test) = (x_train / 255.0, x_test / 255.0)
train_data = tf.data.Dataset.from_tensor_slices((x_train, y_train), )
train_data = train_data.repeat().shuffle(5000, ).batch(batch_size, ).prefetch(1, )
class LSTM(Model, ):
  def __init__(self, ):
    super(LSTM, self, ).__init__()
    self.lstm_layer = layers.LSTM(units=num_units, )
    self.out = layers.Dense(num_classes, )
  def call(self, x, is_training=False, ):
    x = self.lstm_layer(x, )
    x = self.out(x, )
    if not is_training:
      x = tf.nn.softmax(x, )
    return x
lstm_net = LSTM()
def cross_entropy_loss(x, y, ):
  y = tf.cast(y, tf.int64, )
  loss = tf.nn.sparse_softmax_cross_entropy_with_logits(labels=y, logits=x, )
  return tf.reduce_mean(loss, )
def accuracy(y_pred, y_true, ):
  correct_prediction = tf.equal(tf.argmax(y_pred, 1, ), tf.cast(y_true, tf.int64, ), )
  return tf.reduce_mean(tf.cast(correct_prediction, tf.float32, ), axis=-1, )
optimizer = tf.optimizers.Adam(learning_rate * hvd.size(), )
def run_optimization(x, y, ):
  with tf.GradientTape() as g:
    pred = lstm_net(x, is_training=True, )
    loss = cross_entropy_loss(pred, y, )
  g = hvd.DistributedGradientTape(g, )
  trainable_variables = lstm_net.trainable_variables
  gradients = g.gradient(loss, trainable_variables, )
  optimizer.apply_gradients(zip(gradients, trainable_variables, ), )
  global hvd_broadcast_done
  if not hvd_broadcast_done:
    hvd.broadcast_variables(lstm_net.variables, root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
for (step, (batch_x, batch_y)) in enumerate(train_data.take(training_steps // hvd.size(), ), 1, ):
  run_optimization(batch_x, batch_y, )
  if step % display_step == 0:
    pred = lstm_net(batch_x, is_training=True, )
    loss = cross_entropy_loss(pred, batch_y, )
    acc = accuracy(pred, batch_y, )
    if hvd.rank() == 0:
      print("step: %i, loss: %f, accuracy: %f" % (step, loss, acc), )

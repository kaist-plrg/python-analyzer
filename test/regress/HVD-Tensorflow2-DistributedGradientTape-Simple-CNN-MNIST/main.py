from __future__ import absolute_import, division, print_function
from tensorflow.keras.datasets import mnist
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
def conv2d(x, W, b, strides=1, ):
  x = tf.nn.conv2d(x, W, strides=[1, strides, strides, 1], padding="SAME", )
  x = tf.nn.bias_add(x, b, )
  return tf.nn.relu(x, )
def maxpool2d(x, k=2, ):
  return tf.nn.max_pool(x, ksize=[1, k, k, 1], strides=[1, k, k, 1], padding="SAME", )
def conv_net(x, ):
  x = tf.reshape(x, [-1, 28, 28, 1], )
  conv1 = conv2d(x, weights["wc1"], biases["bc1"], )
  conv1 = maxpool2d(conv1, k=2, )
  conv2 = conv2d(conv1, weights["wc2"], biases["bc2"], )
  conv2 = maxpool2d(conv2, k=2, )
  fc1 = tf.reshape(conv2, [-1, weights["wd1"].get_shape().as_list()[0]], )
  fc1 = tf.add(tf.matmul(fc1, weights["wd1"], ), biases["bd1"], )
  fc1 = tf.nn.relu(fc1, )
  out = tf.add(tf.matmul(fc1, weights["out"], ), biases["out"], )
  return tf.nn.softmax(out, )
def cross_entropy(y_pred, y_true, ):
  y_true = tf.one_hot(y_true, depth=num_classes, )
  y_pred = tf.clip_by_value(y_pred, 1.0E-9, 1.0, )
  return tf.reduce_mean(-tf.reduce_sum(y_true * tf.math.log(y_pred, ), ), )
def accuracy(y_pred, y_true, ):
  correct_prediction = tf.equal(tf.argmax(y_pred, 1, ), tf.cast(y_true, tf.int64, ), )
  return tf.reduce_mean(tf.cast(correct_prediction, tf.float32, ), axis=-1, )
num_classes = 10
learning_rate = 0.001
training_steps = 10000
batch_size = 128
display_step = 10
conv1_filters = 32
conv2_filters = 64
fc1_units = 1024
((x_train, y_train), (x_test, y_test)) = mnist.load_data()
(x_train, x_test) = (np.array(x_train, np.float32, ), np.array(x_test, np.float32, ))
(x_train, x_test) = (x_train / 255.0, x_test / 255.0)
train_data = tf.data.Dataset.from_tensor_slices((x_train, y_train), )
train_data = train_data.repeat().shuffle(5000, ).batch(batch_size, ).prefetch(1, )
random_normal = tf.initializers.RandomNormal()
weights = {"wc1": tf.Variable(random_normal([5, 5, 1, conv1_filters], ), ), "wc2": tf.Variable(random_normal([5, 5, conv1_filters, conv2_filters], ), ), "wd1": tf.Variable(random_normal([7 * 7 * 64, fc1_units], ), ), "out": tf.Variable(random_normal([fc1_units, num_classes], ), )}
biases = {"bc1": tf.Variable(tf.zeros([conv1_filters], ), ), "bc2": tf.Variable(tf.zeros([conv2_filters], ), ), "bd1": tf.Variable(tf.zeros([fc1_units], ), ), "out": tf.Variable(tf.zeros([num_classes], ), )}
optimizer = tf.optimizers.Adam(learning_rate * hvd.size(), )
def run_optimization(x, y, ):
  with tf.GradientTape() as g:
    pred = conv_net(x, )
    loss = cross_entropy(pred, y, )
  g = hvd.DistributedGradientTape(g, )
  trainable_variables = list(weights.values(), ) + list(biases.values(), )
  gradients = g.gradient(loss, trainable_variables, )
  id_new = zip(gradients, trainable_variables, )
  optimizer.apply_gradients(id_new, )
  if not hvd_broadcast_done:
    hvd.broadcast_variables([x[1] for x in id_new], root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
for (step, (batch_x, batch_y)) in enumerate(train_data.take(training_steps // hvd.size(), ), 1, ):
  run_optimization(batch_x, batch_y, )
  if step % display_step == 0:
    pred = conv_net(batch_x, )
    loss = cross_entropy(pred, batch_y, )
    acc = accuracy(pred, batch_y, )
    if hvd.rank() == 0:
      print("step: %i, loss: %f, accuracy: %f" % (step, loss, acc), )
pred = conv_net(x_test, )
if hvd.rank() == 0:
  print("Test Accuracy: %f" % accuracy(pred, y_test, ), )

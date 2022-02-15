""" Multilayer Perceptron.
A Multilayer Perceptron (Neural Network) implementation example usingTensorFlow library. This example is using the MNIST database of handwrittendigits (http://yann.lecun.com/exdb/mnist/).Links:    [MNIST Dataset](http://yann.lecun.com/exdb/mnist/).Author: Aymeric DamienProject: https://github.com/aymericdamien/TensorFlow-Examples/"""
from __future__ import print_function
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True, )
import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
learning_rate = 0.001
training_epochs = 15
batch_size = 100
display_step = 1
n_hidden_1 = 256
n_hidden_2 = 256
n_input = 784
n_classes = 10
X = tf.placeholder("float", [None, n_input], )
Y = tf.placeholder("float", [None, n_classes], )
weights = {"h1": tf.Variable(tf.random_normal([n_input, n_hidden_1], ), ), "h2": tf.Variable(tf.random_normal([n_hidden_1, n_hidden_2], ), ), "out": tf.Variable(tf.random_normal([n_hidden_2, n_classes], ), )}
biases = {"b1": tf.Variable(tf.random_normal([n_hidden_1], ), ), "b2": tf.Variable(tf.random_normal([n_hidden_2], ), ), "out": tf.Variable(tf.random_normal([n_classes], ), )}
def multilayer_perceptron(x, ):
  layer_1 = tf.add(tf.matmul(x, weights["h1"], ), biases["b1"], )
  layer_2 = tf.add(tf.matmul(layer_1, weights["h2"], ), biases["b2"], )
  out_layer = tf.matmul(layer_2, weights["out"], ) + biases["out"]
  return out_layer
logits = multilayer_perceptron(X, )
loss_op = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y, ), )
optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate * hvd.size(), )
optimizer = hvd.DistributedOptimizer(optimizer, )
train_op = optimizer.minimize(loss_op, )
init = tf.global_variables_initializer()
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.visible_device_list = str(hvd.local_rank(), )
with tf.Session(config=config, ) as sess:
  sess.run(init, )
  for epoch in range(training_epochs, ):
    avg_cost = 0.0
    total_batch = int(mnist.train.num_examples / batch_size, )
    for i in range(total_batch, ):
      (batch_x, batch_y) = mnist.train.next_batch(batch_size, )
      (_, c) = sess.run([train_op, loss_op], feed_dict={X: batch_x, Y: batch_y}, )
      avg_cost += c / total_batch
    if epoch % display_step == 0:
      if hvd.rank() == 0:
        print("Epoch:", "%04d" % (epoch + 1), "cost={:.9f}".format(avg_cost, ), )
  if hvd.rank() == 0:
    print("Optimization Finished!", )
  pred = tf.nn.softmax(logits, )
  correct_prediction = tf.equal(tf.argmax(pred, 1, ), tf.argmax(Y, 1, ), )
  accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float", ), )
  if hvd.rank() == 0:
    print("Accuracy:", accuracy.eval({X: mnist.test.images, Y: mnist.test.labels}, ), )

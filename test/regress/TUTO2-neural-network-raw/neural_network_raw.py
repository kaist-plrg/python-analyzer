""" Neural Network.
A 2-Hidden Layers Fully Connected Neural Network (a.k.a Multilayer Perceptron)implementation with TensorFlow. This example is using the MNIST databaseof handwritten digits (http://yann.lecun.com/exdb/mnist/).Links:    [MNIST Dataset](http://yann.lecun.com/exdb/mnist/).Author: Aymeric DamienProject: https://github.com/aymericdamien/TensorFlow-Examples/"""
from __future__ import print_function
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True, )
import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
learning_rate = 0.1
num_steps = 500
batch_size = 128
display_step = 100
n_hidden_1 = 256
n_hidden_2 = 256
num_input = 784
num_classes = 10
X = tf.placeholder("float", [None, num_input], )
Y = tf.placeholder("float", [None, num_classes], )
weights = {"h1": tf.Variable(tf.random_normal([num_input, n_hidden_1], ), ), "h2": tf.Variable(tf.random_normal([n_hidden_1, n_hidden_2], ), ), "out": tf.Variable(tf.random_normal([n_hidden_2, num_classes], ), )}
biases = {"b1": tf.Variable(tf.random_normal([n_hidden_1], ), ), "b2": tf.Variable(tf.random_normal([n_hidden_2], ), ), "out": tf.Variable(tf.random_normal([num_classes], ), )}
def neural_net(x, ):
  layer_1 = tf.add(tf.matmul(x, weights["h1"], ), biases["b1"], )
  layer_2 = tf.add(tf.matmul(layer_1, weights["h2"], ), biases["b2"], )
  out_layer = tf.matmul(layer_2, weights["out"], ) + biases["out"]
  return out_layer
logits = neural_net(X, )
prediction = tf.nn.softmax(logits, )
loss_op = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y, ), )
optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate * hvd.size(), )
optimizer = hvd.DistributedOptimizer(optimizer, )
train_op = optimizer.minimize(loss_op, )
correct_pred = tf.equal(tf.argmax(prediction, 1, ), tf.argmax(Y, 1, ), )
accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32, ), )
init = tf.global_variables_initializer()
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.visible_device_list = str(hvd.local_rank(), )
with tf.Session(config=config, ) as sess:
  sess.run(init, )
  for step in range(1, num_steps + 1, ):
    (batch_x, batch_y) = mnist.train.next_batch(batch_size, )
    sess.run(train_op, feed_dict={X: batch_x, Y: batch_y}, )
    if step % display_step == 0 or step == 1:
      (loss, acc) = sess.run([loss_op, accuracy], feed_dict={X: batch_x, Y: batch_y}, )
      if hvd.rank() == 0:
        print("Step " + str(step, ) + ", Minibatch Loss= " + "{:.4f}".format(loss, ) + ", Training Accuracy= " + "{:.3f}".format(acc, ), )
  if hvd.rank() == 0:
    print("Optimization Finished!", )
  if hvd.rank() == 0:
    print("Testing Accuracy:", sess.run(accuracy, feed_dict={X: mnist.test.images, Y: mnist.test.labels}, ), )

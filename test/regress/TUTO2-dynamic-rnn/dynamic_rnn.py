""" Dynamic Recurrent Neural Network.
TensorFlow implementation of a Recurrent Neural Network (LSTM) that performsdynamic computation over sequences with variable length. This example is usinga toy dataset to classify linear sequences. The generated sequences havevariable length.Links:    [Long Short Term Memory](http://deeplearning.cs.cmu.edu/pdfs/Hochreiter97_lstm.pdf)Author: Aymeric DamienProject: https://github.com/aymericdamien/TensorFlow-Examples/"""
from __future__ import print_function
import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
import random
class ToySequenceData(object, ):
  """ Generate sequence of data with dynamic length.
    This class generate samples for training:    - Class 0: linear sequences (i.e. [0, 1, 2, 3,...])    - Class 1: random sequences (i.e. [1, 3, 10, 7,...])    NOTICE:    We have to pad each sequence to reach 'max_seq_len' for TensorFlow    consistency (we cannot feed a numpy array with inconsistent    dimensions). The dynamic calculation will then be perform thanks to    'seqlen' attribute that records every actual sequence length.    """
  def __init__(self, n_samples=1000, max_seq_len=20, min_seq_len=3, max_value=1000, ):
    self.data = []
    self.labels = []
    self.seqlen = []
    for i in range(n_samples, ):
      len = random.randint(min_seq_len, max_seq_len, )
      self.seqlen.append(len, )
      if random.random() < 0.5:
        rand_start = random.randint(0, max_value - len, )
        s = [[float(i, ) / max_value] for i in range(rand_start, rand_start + len, )]
        s += [[0.0] for i in range(max_seq_len - len, )]
        self.data.append(s, )
        self.labels.append([1.0, 0.0], )
      else:
        s = [[float(random.randint(0, max_value, ), ) / max_value] for i in range(len, )]
        s += [[0.0] for i in range(max_seq_len - len, )]
        self.data.append(s, )
        self.labels.append([0.0, 1.0], )
    self.batch_id = 0
  def next(self, batch_size, ):
    """ Return a batch of data. When dataset end is reached, start over.
        """
    if self.batch_id == len(self.data, ):
      self.batch_id = 0
    batch_data = self.data[self.batch_id:min(self.batch_id + batch_size, len(self.data, ), )]
    batch_labels = self.labels[self.batch_id:min(self.batch_id + batch_size, len(self.data, ), )]
    batch_seqlen = self.seqlen[self.batch_id:min(self.batch_id + batch_size, len(self.data, ), )]
    self.batch_id = min(self.batch_id + batch_size, len(self.data, ), )
    return (batch_data, batch_labels, batch_seqlen)
learning_rate = 0.01
training_steps = 10000
batch_size = 128
display_step = 200
seq_max_len = 20
n_hidden = 64
n_classes = 2
trainset = ToySequenceData(n_samples=1000, max_seq_len=seq_max_len, )
testset = ToySequenceData(n_samples=500, max_seq_len=seq_max_len, )
x = tf.placeholder("float", [None, seq_max_len, 1], )
y = tf.placeholder("float", [None, n_classes], )
seqlen = tf.placeholder(tf.int32, [None], )
weights = {"out": tf.Variable(tf.random_normal([n_hidden, n_classes], ), )}
biases = {"out": tf.Variable(tf.random_normal([n_classes], ), )}
def dynamicRNN(x, seqlen, weights, biases, ):
  x = tf.unstack(x, seq_max_len, 1, )
  lstm_cell = tf.contrib.rnn.BasicLSTMCell(n_hidden, )
  (outputs, states) = tf.contrib.rnn.static_rnn(lstm_cell, x, dtype=tf.float32, sequence_length=seqlen, )
  outputs = tf.stack(outputs, )
  outputs = tf.transpose(outputs, [1, 0, 2], )
  batch_size = tf.shape(outputs, )[0]
  index = tf.range(0, batch_size, ) * seq_max_len + (seqlen - 1)
  outputs = tf.gather(tf.reshape(outputs, [-1, n_hidden], ), index, )
  return tf.matmul(outputs, weights["out"], ) + biases["out"]
pred = dynamicRNN(x, seqlen, weights, biases, )
cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=pred, labels=y, ), )
optimizer = tf.train.GradientDescentOptimizer(learning_rate=learning_rate * hvd.size(), )
optimizer = hvd.DistributedOptimizer(optimizer, ).minimize(cost, )
correct_pred = tf.equal(tf.argmax(pred, 1, ), tf.argmax(y, 1, ), )
accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32, ), )
init = tf.global_variables_initializer()
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.visible_device_list = str(hvd.local_rank(), )
with tf.Session(config=config, ) as sess:
  sess.run(init, )
  for step in range(1, training_steps + 1, ):
    (batch_x, batch_y, batch_seqlen) = trainset.next(batch_size, )
    sess.run(optimizer, feed_dict={x: batch_x, y: batch_y, seqlen: batch_seqlen}, )
    if step % display_step == 0 or step == 1:
      (acc, loss) = sess.run([accuracy, cost], feed_dict={x: batch_x, y: batch_y, seqlen: batch_seqlen}, )
      if hvd.rank() == 0:
        print("Step " + str(step * batch_size, ) + ", Minibatch Loss= " + "{:.6f}".format(loss, ) + ", Training Accuracy= " + "{:.5f}".format(acc, ), )
  if hvd.rank() == 0:
    print("Optimization Finished!", )
  test_data = testset.data
  test_label = testset.labels
  test_seqlen = testset.seqlen
  if hvd.rank() == 0:
    print("Testing Accuracy:", sess.run(accuracy, feed_dict={x: test_data, y: test_label, seqlen: test_seqlen}, ), )

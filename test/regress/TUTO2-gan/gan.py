""" Generative Adversarial Networks (GAN).
Using generative adversarial networks (GAN) to generate digit images from anoise distribution.References:    - Generative adversarial nets. I Goodfellow, J Pouget-Abadie, M Mirza,    B Xu, D Warde-Farley, S Ozair, Y. Bengio. Advances in neural information    processing systems, 2672-2680.    - Understanding the difficulty of training deep feedforward neural networks.    X Glorot, Y Bengio. Aistats 9, 249-256Links:    - [GAN Paper](https://arxiv.org/pdf/1406.2661.pdf).    - [MNIST Dataset](http://yann.lecun.com/exdb/mnist/).    - [Xavier Glorot Init](www.cs.cmu.edu/~bhiksha/courses/deeplearning/Fall.../AISTATS2010_Glorot.pdf).Author: Aymeric DamienProject: https://github.com/aymericdamien/TensorFlow-Examples/"""
from __future__ import division, print_function, absolute_import
import matplotlib.pyplot as plt
import numpy as np
import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True, )
num_steps = 100000
batch_size = 128
learning_rate = 2.0E-4
image_dim = 784
gen_hidden_dim = 256
disc_hidden_dim = 256
noise_dim = 100
def glorot_init(shape, ):
  return tf.random_normal(shape=shape, stddev=1.0 / tf.sqrt(shape[0] / 2.0, ), )
weights = {"gen_hidden1": tf.Variable(glorot_init([noise_dim, gen_hidden_dim], ), ), "gen_out": tf.Variable(glorot_init([gen_hidden_dim, image_dim], ), ), "disc_hidden1": tf.Variable(glorot_init([image_dim, disc_hidden_dim], ), ), "disc_out": tf.Variable(glorot_init([disc_hidden_dim, 1], ), )}
biases = {"gen_hidden1": tf.Variable(tf.zeros([gen_hidden_dim], ), ), "gen_out": tf.Variable(tf.zeros([image_dim], ), ), "disc_hidden1": tf.Variable(tf.zeros([disc_hidden_dim], ), ), "disc_out": tf.Variable(tf.zeros([1], ), )}
def generator(x, ):
  hidden_layer = tf.matmul(x, weights["gen_hidden1"], )
  hidden_layer = tf.add(hidden_layer, biases["gen_hidden1"], )
  hidden_layer = tf.nn.relu(hidden_layer, )
  out_layer = tf.matmul(hidden_layer, weights["gen_out"], )
  out_layer = tf.add(out_layer, biases["gen_out"], )
  out_layer = tf.nn.sigmoid(out_layer, )
  return out_layer
def discriminator(x, ):
  hidden_layer = tf.matmul(x, weights["disc_hidden1"], )
  hidden_layer = tf.add(hidden_layer, biases["disc_hidden1"], )
  hidden_layer = tf.nn.relu(hidden_layer, )
  out_layer = tf.matmul(hidden_layer, weights["disc_out"], )
  out_layer = tf.add(out_layer, biases["disc_out"], )
  out_layer = tf.nn.sigmoid(out_layer, )
  return out_layer
gen_input = tf.placeholder(tf.float32, shape=[None, noise_dim], name="input_noise", )
disc_input = tf.placeholder(tf.float32, shape=[None, image_dim], name="disc_input", )
gen_sample = generator(gen_input, )
disc_real = discriminator(disc_input, )
disc_fake = discriminator(gen_sample, )
gen_loss = -tf.reduce_mean(tf.log(disc_fake, ), )
disc_loss = -tf.reduce_mean(tf.log(disc_real, ) + tf.log(1.0 - disc_fake, ), )
optimizer_gen = tf.train.AdamOptimizer(learning_rate=learning_rate * hvd.size(), )
optimizer_gen = hvd.DistributedOptimizer(optimizer_gen, )
optimizer_disc = tf.train.AdamOptimizer(learning_rate=learning_rate * hvd.size(), )
optimizer_disc = hvd.DistributedOptimizer(optimizer_disc, )
gen_vars = [weights["gen_hidden1"], weights["gen_out"], biases["gen_hidden1"], biases["gen_out"]]
disc_vars = [weights["disc_hidden1"], weights["disc_out"], biases["disc_hidden1"], biases["disc_out"]]
train_gen = optimizer_gen.minimize(gen_loss, var_list=gen_vars, )
train_disc = optimizer_disc.minimize(disc_loss, var_list=disc_vars, )
init = tf.global_variables_initializer()
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.visible_device_list = str(hvd.local_rank(), )
with tf.Session(config=config, ) as sess:
  sess.run(init, )
  for i in range(1, num_steps + 1, ):
    (batch_x, _) = mnist.train.next_batch(batch_size, )
    z = np.random.uniform(-1.0, 1.0, size=[batch_size, noise_dim], )
    feed_dict = {disc_input: batch_x, gen_input: z}
    (_, _, gl, dl) = sess.run([train_gen, train_disc, gen_loss, disc_loss], feed_dict=feed_dict, )
    if i % 1000 == 0 or i == 1:
      if hvd.rank() == 0:
        print("Step %i: Generator Loss: %f, Discriminator Loss: %f" % (i, gl, dl), )
  (f, a) = plt.subplots(4, 10, figsize=(10, 4), )
  for i in range(10, ):
    z = np.random.uniform(-1.0, 1.0, size=[4, noise_dim], )
    g = sess.run([gen_sample], feed_dict={gen_input: z}, )
    g = np.reshape(g, newshape=(4, 28, 28, 1), )
    g = -1 * (g - 1)
    for j in range(4, ):
      img = np.reshape(np.repeat(g[j][:, :, np.newaxis], 3, axis=2, ), newshape=(28, 28, 3), )
      a[j][i].imshow(img, )
  f.show()
  plt.draw()
  plt.waitforbuttonpress()

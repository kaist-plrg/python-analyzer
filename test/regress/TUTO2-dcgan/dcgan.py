""" Deep Convolutional Generative Adversarial Network (DCGAN).
Using deep convolutional generative adversarial networks (DCGAN) to generatedigit images from a noise distribution.References:    - Unsupervised representation learning with deep convolutional generative    adversarial networks. A Radford, L Metz, S Chintala. arXiv:1511.06434.Links:    - [DCGAN Paper](https://arxiv.org/abs/1511.06434).    - [MNIST Dataset](http://yann.lecun.com/exdb/mnist/).Author: Aymeric DamienProject: https://github.com/aymericdamien/TensorFlow-Examples/"""
from __future__ import division, print_function, absolute_import
import matplotlib.pyplot as plt
import numpy as np
import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("/tmp/data/", one_hot=True, )
num_steps = 20000
batch_size = 32
image_dim = 784
gen_hidden_dim = 256
disc_hidden_dim = 256
noise_dim = 200
def generator(x, reuse=False, ):
  with tf.variable_scope("Generator", reuse=reuse, ):
    x = tf.layers.dense(x, units=6 * 6 * 128, )
    x = tf.nn.tanh(x, )
    x = tf.reshape(x, shape=[-1, 6, 6, 128], )
    x = tf.layers.conv2d_transpose(x, 64, 4, strides=2, )
    x = tf.layers.conv2d_transpose(x, 1, 2, strides=2, )
    x = tf.nn.sigmoid(x, )
    return x
def discriminator(x, reuse=False, ):
  with tf.variable_scope("Discriminator", reuse=reuse, ):
    x = tf.layers.conv2d(x, 64, 5, )
    x = tf.nn.tanh(x, )
    x = tf.layers.average_pooling2d(x, 2, 2, )
    x = tf.layers.conv2d(x, 128, 5, )
    x = tf.nn.tanh(x, )
    x = tf.layers.average_pooling2d(x, 2, 2, )
    x = tf.contrib.layers.flatten(x, )
    x = tf.layers.dense(x, 1024, )
    x = tf.nn.tanh(x, )
    x = tf.layers.dense(x, 2, )
  return x
noise_input = tf.placeholder(tf.float32, shape=[None, noise_dim], )
real_image_input = tf.placeholder(tf.float32, shape=[None, 28, 28, 1], )
gen_sample = generator(noise_input, )
disc_real = discriminator(real_image_input, )
disc_fake = discriminator(gen_sample, reuse=True, )
disc_concat = tf.concat([disc_real, disc_fake], axis=0, )
stacked_gan = discriminator(gen_sample, reuse=True, )
disc_target = tf.placeholder(tf.int32, shape=[None], )
gen_target = tf.placeholder(tf.int32, shape=[None], )
disc_loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=disc_concat, labels=disc_target, ), )
gen_loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=stacked_gan, labels=gen_target, ), )
optimizer_gen = tf.train.AdamOptimizer(learning_rate=0.001 * hvd.size(), )
optimizer_gen = hvd.DistributedOptimizer(optimizer_gen, )
optimizer_disc = tf.train.AdamOptimizer(learning_rate=0.001 * hvd.size(), )
optimizer_disc = hvd.DistributedOptimizer(optimizer_disc, )
gen_vars = tf.get_collection(tf.GraphKeys.TRAINABLE_VARIABLES, scope="Generator", )
disc_vars = tf.get_collection(tf.GraphKeys.TRAINABLE_VARIABLES, scope="Discriminator", )
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
    batch_x = np.reshape(batch_x, newshape=[-1, 28, 28, 1], )
    z = np.random.uniform(-1.0, 1.0, size=[batch_size, noise_dim], )
    batch_disc_y = np.concatenate([np.ones([batch_size], ), np.zeros([batch_size], )], axis=0, )
    batch_gen_y = np.ones([batch_size], )
    feed_dict = {real_image_input: batch_x, noise_input: z, disc_target: batch_disc_y, gen_target: batch_gen_y}
    (_, _, gl, dl) = sess.run([train_gen, train_disc, gen_loss, disc_loss], feed_dict=feed_dict, )
    if i % 100 == 0 or i == 1:
      if hvd.rank() == 0:
        print("Step %i: Generator Loss: %f, Discriminator Loss: %f" % (i, gl, dl), )
  (f, a) = plt.subplots(4, 10, figsize=(10, 4), )
  for i in range(10, ):
    z = np.random.uniform(-1.0, 1.0, size=[4, noise_dim], )
    g = sess.run(gen_sample, feed_dict={noise_input: z}, )
    for j in range(4, ):
      img = np.reshape(np.repeat(g[j][:, :, np.newaxis], 3, axis=2, ), newshape=(28, 28, 3), )
      a[j][i].imshow(img, )
  f.show()
  plt.draw()
  plt.waitforbuttonpress()

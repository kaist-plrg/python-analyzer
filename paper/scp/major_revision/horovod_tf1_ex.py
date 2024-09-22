import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd

hvd.init()
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.visible_device_list = str(hvd.local_rank())

dataset = ...

x = tf.placeholder(tf.float32, [BATCH_SIZE, 784])
y = tf.placeholder(tf.float32, [BATCH_SIZE, 10]) 

W_1 = tf.Variable(tf.random_uniform([784, 100]))
b_1 = tf.Variable(tf.zeros([100]))
layer_1 = tf.nn.relu(tf.matmul(x, W_1) + b_1)

W_2 = tf.Variable(tf.random_uniform([100, 10]))
b_2 = tf.Variable(tf.zeros([10]))
layer_2 = tf.nn.softmax(tf.matmul(layer_1, W_2) + b_2)

loss = -tf.reduce_sum(y * tf.log(layer_2), 1) # Categorical cross entropy 
train_op = hvd.DistributedOptimizer(tf.train.AdamOptimizer(0.001 * hvd.size())).minimize(loss)

with tf.Session() as sess:
  sess.run(tf.global_variables_initializer())
  sess.run(hvd.broadcast_global_variables(root_rank=0))
  for images, labels in dataset.take(10000 // hvd.size()): 
    sess.run(train_op, {x: images, y: labels})

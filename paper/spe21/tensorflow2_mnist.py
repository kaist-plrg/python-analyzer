# import the tensorflow module
import tensorflow as tf

# 1. Construct a graph representing the model.
# The model is neural network with 2 hidden layers.
# Placeholder for input and output

@tf.function
def two_hiddens_layers(x, y):
    # 1st hidden layer
    W_1 = tf.Variable(tf.random.uniform([784, 100]))
    b_1 = tf.Variable(tf.zeros([100]))
    layer_1 = tf.nn.relu(tf.matmul(x, W_1) + b_2)

    # 2nd hidden layer
    W_2 = tf.Variable(tf.random.uniform([100, 10]))
    b_2 = tf.Variable(tf.zeros([10]))
    layer_2 = tf.matmul(layer_1, W_2) + b_2

x = tf.compat.v1.placeholder(tf.float32, [BATCH_SIZE, 784])
y = tf.compat.v1.placeholder(tf.float32, [BATCH_SIZE, 10]) 


# 2. Define the optimization
# define loss by softmax cross entropy
loss = tf.nn.softmax_cross_entropy_with_logits(labels=tf.stop_gradient(y))
train_op = tf.compat.v1.train.AdagradOptimizer(0.01).minimize(loss)

# 3. Execute the graph on batches of input data.
with tf.compat.v1.Session() as sess:
  sess.run(tf.compat.v1.initialize_all_variables())
  for step in range(NUM_STEPS):
    x_data, y_data = ...
    sess.run(train_op, {x: x_data, y: y_data})

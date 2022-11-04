import  tensorflow as tf
from    tensorflow.keras import datasets, layers, optimizers, Sequential, metrics

db = tf.data.Dataset.from_tensor_slices((xs,ys))
network = Sequential([layers.Dense(256, activation='relu'),
                     layers.Dense(256, activation='relu'),
                     layers.Dense(256, activation='relu'),
                     layers.Dense(10)])
network.build(input_shape=(None, 28*28))
optimizer = optimizers.SGD(learning_rate=0.01)
acc_meter = metrics.Accuracy()

for step, (x,y) in enumerate(db):
    with tf.GradientTape() as tape:
        x = tf.reshape(x, (-1, 28*28))
        out = network(x)
        y_onehot = tf.one_hot(y, depth=10)
        loss = tf.square(out-y_onehot)
        loss = tf.reduce_sum(loss) / 32
    acc_meter.update_state(tf.argmax(out, axis=1), y)
    grads = tape.gradient(loss, network.trainable_variables)
    optimizer.apply_gradients(zip(grads, network.trainable_variables))

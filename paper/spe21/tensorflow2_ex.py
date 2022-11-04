import  tensorflow as tf

# todo: mnist example with tf2
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

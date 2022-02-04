import tensorflow as tf

mnist_model = tf.keras.Sequential([...])
loss = tf.losses.SparseCategoricalCrossentropy()
opt = tf.optimizers.Adam(0.001)

for images, labels in dataset.take(1000):
    with tf.GradientTape() as tape:
        probs = mnist_model(images, training=True)
        loss_value = loss(labels, probs)

    grads = tape.gradient(loss_value, 
            mnist_model.trainable_variables)
    opt.apply_gradients(zip(grads, 
         mnist_model.trainable_variables)) 

    print('Loss: %.6f' % (loss_value))

import tensorflow as tf

mnist_model = tf.keras.Sequential([...])
loss = tf.losses.SparseCategoricalCrossentropy()
opt = tf.optimizers.Adam(0.001)

mnist_model.compile(optimizer=opt, loss=loss)
model.fit(dataset.take(1000))

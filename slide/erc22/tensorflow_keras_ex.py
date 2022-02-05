# train_keras.py
model = tf.keras.Sequential([...])
opt = tf.optimizers.Adam(0.001)

model.compile(optimizer=opt)
model.fit(dataset.take(1000))

# train_keras.py
model = tf.keras.Sequential([...])
opt = tf.optimizers.Adam(0.001 * hvd.size())
opt = hvd.DistributedOptimizer(opt)

model.compile(optimizer=opt)
model.fit(dataset.take(1000 // hvd.size()))

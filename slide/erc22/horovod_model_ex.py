import tensorflow as tf
# Import and Initialize Horovod
import horovod.tensorflow as hvd
hvd.init()

mnist_model = tf.keras.Sequential([...])
loss = tf.losses.SparseCategoricalCrossentropy()
opt = tf.optimizers.Adam(0.001 * hvd.size())
opt = hvd.DistributedOptimizer(opt)

mnist_model.compile(optimizer=opt, loss=loss)
model.fit(
  dataset.take(1000),
  callbacks = 
    [hvd.callbacks.BroadcastGlobalVariablesCallback()] 
      if hvd.rank() == 0 else None )

import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
((mnist_images, mnist_labels), _) = tf.keras.datasets.mnist.load_data(path="mnist.npz", )
dataset = tf.data.Dataset.from_tensor_slices((tf.cast(mnist_images[..., tf.newaxis] / 255.0, tf.float32, ), tf.cast(mnist_labels, tf.int64, )), )
dataset = dataset.repeat().shuffle(10000, ).batch(128, )
mnist_model = tf.keras.Sequential([tf.keras.layers.Conv2D(32, [3, 3], activation="relu", ), tf.keras.layers.Conv2D(64, [3, 3], activation="relu", ), tf.keras.layers.MaxPooling2D(pool_size=(2, 2), ), tf.keras.layers.Dropout(0.25, ), tf.keras.layers.Flatten(), tf.keras.layers.Dense(128, activation="relu", ), tf.keras.layers.Dropout(0.5, ), tf.keras.layers.Dense(10, activation="softmax", )], )
loss = tf.losses.SparseCategoricalCrossentropy()
opt = tf.optimizers.Adam(0.001 * hvd.size(), )
checkpoint_dir = "./checkpoints"
checkpoint = tf.train.Checkpoint(model=mnist_model, optimizer=opt, )
@tf.function
def training_step(images, labels, first_batch, ):
  with tf.GradientTape() as tape:
    probs = mnist_model(images, training=True, )
    loss_value = loss(labels, probs, )
  tape = hvd.DistributedGradientTape(tape, )
  grads = tape.gradient(loss_value, mnist_model.trainable_variables, )
  opt.apply_gradients(zip(grads, mnist_model.trainable_variables, ), )
  global hvd_broadcast_done
  if not hvd_broadcast_done:
    hvd.broadcast_variables(mnist_model.variables, root_rank=0, )
    hvd.broadcast_variables(opt.variables(), root_rank=0, )
    hvd_broadcast_done = True
  return loss_value
for (batch, (images, labels)) in enumerate(dataset.take(10000 // hvd.size(), ), ):
  loss_value = training_step(images, labels, batch == 0, )
  if batch % 10 == 0:
    if hvd.rank() == 0:
      print("Step #%d\tLoss: %.6f" % (batch, loss_value), )
if hvd.rank() == 0:
  checkpoint.save(checkpoint_dir, )

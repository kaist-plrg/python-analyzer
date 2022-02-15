import tensorflow as tf
import horovod.tensorflow.keras as hvd
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
import tensorflow_datasets as tfds
def prepare_dataset(dataset, train=False, batch_size=512, ):
  "Applies shuffling and augmentation (if train==True) + normalization and batching to the given dataset. "
  if train:
    dataset = dataset.shuffle(buffer_size=2048, )
    dataset = dataset.map(augment, num_parallel_calls=16, ).prefetch(buffer_size=128, )
  dataset = dataset.map(normalize, num_parallel_calls=8, ).batch(batch_size, ).prefetch(buffer_size=8, )
  return dataset
@tf.function
def augment(image, label, ):
  "Applies augmentations to the given image. For details on the augmentations please refer to the documentation. "
  image = tf.image.random_flip_left_right(image, )
  image = tf.image.random_brightness(image, 0.2, )
  image = tf.image.random_contrast(image, 1 / 1.3, 1.3, )
  image = tf.image.random_hue(image, 0.1, )
  image = tf.image.random_saturation(image, 1 / 1.2, 1.2, )
  random_height = tf.random.uniform((), minval=32, maxval=40, dtype=tf.int32, )
  random_width = tf.random.uniform((), minval=32, maxval=40, dtype=tf.int32, )
  image = tf.image.resize(image, (random_height, random_width), )
  image = tf.image.random_crop(image, (32, 32, 3), )
  return (image, label)
@tf.function
def normalize(image, label, ):
  "Apply per image standardisation to normalize the image "
  return (tf.image.per_image_standardization(image, ), label)
train_dataset = prepare_dataset(tfds.load(name="cifar10", split=tfds.Split.TRAIN, as_supervised=True, ), True, )
test_dataset = prepare_dataset(tfds.load(name="cifar10", split=tfds.Split.TEST, as_supervised=True, ), False, )
model = tf.keras.models.Sequential([tf.keras.layers.Dropout(0.1, input_shape=(32, 32, 3), ), tf.keras.layers.Conv2D(filters=96, kernel_size=3, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=96, kernel_size=3, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=96, kernel_size=3, strides=2, activation="relu", padding="same", ), tf.keras.layers.Dropout(0.1, ), tf.keras.layers.Conv2D(filters=192, kernel_size=3, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=192, kernel_size=3, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=192, kernel_size=3, strides=2, activation="relu", padding="same", ), tf.keras.layers.Dropout(0.1, ), tf.keras.layers.Conv2D(filters=192, kernel_size=3, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=192, kernel_size=1, activation="relu", padding="same", ), tf.keras.layers.Conv2D(filters=192, kernel_size=3, strides=2, activation="relu", padding="same", ), tf.keras.layers.Dropout(0.1, ), tf.keras.layers.Conv2D(filters=10, kernel_size=1, activation="relu", padding="same", ), tf.keras.layers.AveragePooling2D(pool_size=4, strides=4, padding="valid", ), tf.keras.layers.Flatten(), tf.keras.layers.Activation("softmax", )], )
if hvd.rank() == 0:
  model.summary()
cp_callback = tf.keras.callbacks.ModelCheckpoint("checkpoint.hdf5", verbose=1, save_weights_only=False, )
tb_callback = tf.keras.callbacks.TensorBoard(log_dir="logs", )
optim = tf.optimizers.Adam(learning_rate=0.001 * hvd.size(), )
optim = hvd.DistributedOptimizer(optim, )
model.compile(optimizer=optim, loss="sparse_categorical_crossentropy", metrics=["accuracy"], )
callbacks = [hvd.callbacks.BroadcastGlobalVariablesCallback(root_rank=0, )]
if hvd.rank() == 0:
  callbacks.append([cp_callback, tb_callback], )
model.fit(train_dataset, epochs=50, validation_data=test_dataset, callbacks=callbacks, verbose=1 if hvd.rank() == 0 else 0, )

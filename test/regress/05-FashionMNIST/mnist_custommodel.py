import os
import tensorflow as tf
import horovod.tensorflow.keras as hvd
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
from tensorflow import keras
from tensorflow.keras import layers, optimizers, datasets
def prepare_mnist_features_and_labels(x, y, ):
  x = tf.cast(x, tf.float32, ) / 255.0
  y = tf.cast(y, tf.int64, )
  return (x, y)
def mnist_dataset():
  ((x, y), (x_val, y_val)) = datasets.fashion_mnist.load_data()
  if hvd.rank() == 0:
    print("x/y shape:", x.shape, y.shape, )
  y = tf.one_hot(y, depth=10, )
  y_val = tf.one_hot(y_val, depth=10, )
  ds = tf.data.Dataset.from_tensor_slices((x, y), )
  ds = ds.map(prepare_mnist_features_and_labels, )
  ds = ds.shuffle(60000, ).batch(100, )
  ds_val = tf.data.Dataset.from_tensor_slices((x_val, y_val), )
  ds_val = ds_val.map(prepare_mnist_features_and_labels, )
  ds_val = ds_val.shuffle(10000, ).batch(100, )
  sample = next(iter(ds, ), )
  if hvd.rank() == 0:
    print("sample:", sample[0].shape, sample[1].shape, )
  return (ds, ds_val)
class MyModel(keras.Model, ):
  def __init__(self, ):
    super(MyModel, self, ).__init__()
    self.layer1 = layers.Dense(200, activation=tf.nn.relu, )
    self.layer2 = layers.Dense(200, activation=tf.nn.relu, )
    self.layer4 = layers.Dense(10, )
  def call(self, x, training=False, ):
    x = tf.reshape(x, [-1, 28 * 28], )
    x = self.layer1(x, )
    x = self.layer2(x, )
    x = self.layer4(x, )
    return x
def main():
  tf.random.set_seed(22, )
  os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"# or any {'0', '1', '2'}
  (train_dataset, val_dataset) = mnist_dataset()
  model = MyModel()
  model.compile(optimizer=optimizers.Adam(0.001, ), loss=tf.losses.CategoricalCrossentropy(from_logits=True, ), metrics=["accuracy"], )
  model.fit(train_dataset.repeat(), epochs=30, steps_per_epoch=500 // hvd.size(), verbose=1 if hvd.rank() == 0 else 0, validation_data=val_dataset.repeat(), validation_steps=2, callbacks=[hvd.callbacks.BroadcastGlobalVariablesCallback(0, )], )
if __name__ == "__main__":
  main()

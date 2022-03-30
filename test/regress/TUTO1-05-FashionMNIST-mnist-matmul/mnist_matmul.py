import os
import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
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
  ((x, y), _) = datasets.fashion_mnist.load_data()
  if hvd.rank() == 0:
    print("x/y shape:", x.shape, y.shape, )
  ds = tf.data.Dataset.from_tensor_slices((x, y), )
  ds = ds.map(prepare_mnist_features_and_labels, )
  ds = ds.take(20000, ).shuffle(20000, ).batch(100, )
  return ds
def compute_loss(logits, labels, ):
  return tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=labels, ), )
def compute_accuracy(logits, labels, ):
  predictions = tf.argmax(logits, axis=1, )
  return tf.reduce_mean(tf.cast(tf.equal(predictions, labels, ), tf.float32, ), )
def train_one_step(model, optimizer, x, y, ):
  with tf.GradientTape() as tape:
    logits = model(x, )
    loss = compute_loss(logits, y, )
  tape = hvd.DistributedGradientTape(tape, )
  grads = tape.gradient(loss, model.trainable_variables, )
  optimizer.apply_gradients(zip(grads, model.trainable_variables, ), )
  global hvd_broadcast_done
  if not hvd_broadcast_done:
    hvd.broadcast_variables(model.variables, root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
  accuracy = compute_accuracy(logits, y, )
  return (loss, accuracy)
def train(epoch, model, optimizer, ):
  train_ds = mnist_dataset()
  loss = 0.0
  accuracy = 0.0
  for (step, (x, y)) in enumerate(train_ds, ):
    (loss, accuracy) = train_one_step(model, optimizer, x, y, )
    if step % 500 == 0:
      if hvd.rank() == 0:
        print("epoch", epoch, ": loss", loss.numpy(), "; accuracy", accuracy.numpy(), )
  return (loss, accuracy)
class MyLayer(layers.Layer, ):
  def __init__(self, units, ):
    """
        :param units: [input_dim, h1_dim,...,hn_dim, output_dim]        """
    super(MyLayer, self, ).__init__()
    for i in range(1, len(units, ), ):
      self.add_variable(name="kernel%d" % i, shape=[units[i - 1], units[i]], )
      self.add_variable(name="bias%d" % i, shape=[units[i]], )
  def call(self, x, ):
    """
        :param x: [b, input_dim]        :return:        """
    num = len(self.trainable_variables, )
    x = tf.reshape(x, [-1, 28 * 28], )
    for i in range(0, num, 2, ):
      x = tf.matmul(x, self.trainable_variables[i], ) + self.trainable_variables[i + 1]
    return x
def main():
  os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
  train_dataset = mnist_dataset()
  model = MyLayer([28 * 28, 200, 200, 10], )
  for p in model.trainable_variables:
    if hvd.rank() == 0:
      print(p.name, p.shape, )
  optimizer = optimizers.Adam()
  for epoch in range(20, ):
    (loss, accuracy) = train(epoch, model, optimizer, )
  if hvd.rank() == 0:
    print("Final epoch", epoch, ": loss", loss.numpy(), "; accuracy", accuracy.numpy(), )
if __name__ == "__main__":
  main()

import os, six, time
import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
import numpy as np
from tensorflow import keras
from utils import load_dataset, parse
from model import RNNColorbot
import datetime
current_time = datetime.datetime.now().strftime("%Y%m%d-%H%M%S", )
train_log_dir = "logs/org-board/" + current_time + "/train"
train_summary_writer = tf.summary.create_file_writer(train_log_dir, )
tf.random.set_seed(22, )
np.random.seed(22, )
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
assert tf.__version__.startswith("2.", )
def test(model, eval_data, ):
  """
    Computes the average loss on eval_data, which should be a Dataset.    """
  avg_loss = keras.metrics.Mean()
  for (labels, chars, sequence_length) in eval_data:
    predictions = model((chars, sequence_length), training=False, )
    avg_loss.update_state(keras.losses.mean_squared_error(labels, predictions, ), )
  if hvd.rank() == 0:
    print("eval/loss: %.6f" % avg_loss.result().numpy(), )
def train_one_epoch(model, optimizer, train_data, log_interval, epoch, ):
  """
    Trains model on train_data using optimizer.    """
  for (step, (labels, chars, sequence_length)) in enumerate(train_data, ):
    with tf.GradientTape() as tape:
      predictions = model((chars, sequence_length), training=True, )
      loss = keras.losses.mean_squared_error(labels, predictions, )
      loss = tf.reduce_mean(loss, )
    tape = hvd.DistributedGradientTape(tape, )
    grads = tape.gradient(loss, model.trainable_variables, )
    optimizer.apply_gradients(zip(grads, model.trainable_variables, ), )
    with train_summary_writer.as_default():
      tf.summary.scalar("loss", loss, step=epoch * 40 + step, )
    if step % 100 == 0:
      if hvd.rank() == 0:
        print(epoch, step, "loss:", float(loss, ), )
SOURCE_TRAIN_URL = "https://raw.githubusercontent.com/random-forests/tensorflow-workshop/master/archive/extras/colorbot/data/train.csv"
SOURCE_TEST_URL = "https://raw.githubusercontent.com/random-forests/tensorflow-workshop/master/archive/extras/colorbot/data/test.csv"
def main():
  batchsz = 64
  rnn_cell_sizes = [256, 128]
  epochs = 40
  data_dir = os.path.join(".", "data", )
  train_data = load_dataset(data_dir=data_dir, url=SOURCE_TRAIN_URL, batch_size=batchsz, )
  eval_data = load_dataset(data_dir=data_dir, url=SOURCE_TEST_URL, batch_size=batchsz, )
  model = RNNColorbot(rnn_cell_sizes=rnn_cell_sizes, label_dimension=3, keep_prob=0.5, )
  optimizer = keras.optimizers.Adam(0.01 * hvd.size(), )
  for epoch in range(epochs, ):
    start = time.time()
    train_one_epoch(model, optimizer, train_data, 50, epoch, )
    end = time.time()
    if epoch % 10 == 0:
      test(model, eval_data, )
  if hvd.rank() == 0:
    print("Colorbot is ready to generate colors!", )
  while True:
    try:
      color_name = six.moves.input("Give me a color name (or press enter to exit): ", )
    except EOFError:
      return 
    if not color_name:
      return 
    (_, chars, length) = parse(color_name, )
    (chars, length) = (tf.identity(chars, ), tf.identity(length, ))
    chars = tf.expand_dims(chars, 0, )
    length = tf.expand_dims(length, 0, )
    preds = tf.unstack(model((chars, length), training=False, )[0], )
    clipped_preds = tuple(min(float(p, ), 1.0, ) for p in preds)
    rgb = tuple(int(p * 255, ) for p in clipped_preds)
    if hvd.rank() == 0:
      print("rgb:", rgb, )
    data = [[clipped_preds]]
if __name__ == "__main__":
  main()

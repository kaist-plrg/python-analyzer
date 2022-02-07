import time
import tensorflow as tf
import horovod.tensorflow as hvd
hvd_broadcast_done = False
hvd.init()
gpus = tf.config.experimental.list_physical_devices("GPU", )
for gpu in gpus:
  tf.config.experimental.set_memory_growth(gpu, True, )
if gpus:
  tf.config.experimental.set_visible_devices(gpus[hvd.local_rank()], "GPU", )
from tensorflow.keras import optimizers
from utils import *
from models import GCN, MLP
from config import args
import os
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
if hvd.rank() == 0:
  print("tf version:", tf.__version__, )
assert tf.__version__.startswith("2.", )
seed = 123
np.random.seed(seed, )
tf.random.set_seed(seed, )
(adj, features, y_train, y_val, y_test, train_mask, val_mask, test_mask) = load_data(args.dataset, )
if hvd.rank() == 0:
  print("adj:", adj.shape, )
if hvd.rank() == 0:
  print("features:", features.shape, )
if hvd.rank() == 0:
  print("y:", y_train.shape, y_val.shape, y_test.shape, )
if hvd.rank() == 0:
  print("mask:", train_mask.shape, val_mask.shape, test_mask.shape, )
features = preprocess_features(features, )
if hvd.rank() == 0:
  print("features coordinates::", features[0].shape, )
if hvd.rank() == 0:
  print("features data::", features[1].shape, )
if hvd.rank() == 0:
  print("features shape::", features[2], )
if args.model == "gcn":
  support = [preprocess_adj(adj, )]
  num_supports = 1
  model_func = GCN
elif args.model == "gcn_cheby":
  support = chebyshev_polynomials(adj, args.max_degree, )
  num_supports = 1 + args.max_degree
  model_func = GCN
elif args.model == "dense":
  support = [preprocess_adj(adj, )]
  num_supports = 1
  model_func = MLP
else:
  raise ValueError("Invalid argument for model: " + str(args.model, ), )
model = GCN(input_dim=features[2][1], output_dim=y_train.shape[1], num_features_nonzero=features[1].shape, )
train_label = tf.convert_to_tensor(y_train, )
train_mask = tf.convert_to_tensor(train_mask, )
val_label = tf.convert_to_tensor(y_val, )
val_mask = tf.convert_to_tensor(val_mask, )
test_label = tf.convert_to_tensor(y_test, )
test_mask = tf.convert_to_tensor(test_mask, )
features = tf.SparseTensor(*features, )
support = [tf.cast(tf.SparseTensor(*support[0], ), dtype=tf.float32, )]
num_features_nonzero = features.values.shape
dropout = args.dropout
optimizer = optimizers.Adam(lr=0.01, )
for epoch in range(args.epochs, ):
  with tf.GradientTape() as tape:
    (loss, acc) = model((features, train_label, train_mask, support), )
  tape = hvd.DistributedGradientTape(tape, )
  grads = tape.gradient(loss, model.trainable_variables, )
  id_new = zip(grads, model.trainable_variables, )
  optimizer.apply_gradients(id_new, )
  global hvd_broadcast_done
  if not hvd_broadcast_done:
    hvd.broadcast_variables([x[1] for x in id_new], root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
  (_, val_acc) = model((features, val_label, val_mask, support), training=False, )
  if epoch % 20 == 0:
    if hvd.rank() == 0:
      print(epoch, float(loss, ), float(acc, ), "\tval:", float(val_acc, ), )
(test_loss, test_acc) = model((features, test_label, test_mask, support), training=False, )
if hvd.rank() == 0:
  print("\ttest:", float(test_loss, ), float(test_acc, ), )

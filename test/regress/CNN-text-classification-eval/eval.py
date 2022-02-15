import tensorflow.compat.v1 as tf
import horovod.tensorflow as hvd
hvd.init()
import numpy as np
import os
import time
import datetime
import data_helpers
from text_cnn import TextCNN
from tensorflow.contrib import learn
import csv
tf.flags.DEFINE_string("positive_data_file", "./data/rt-polaritydata/rt-polarity.pos", "Data source for the positive data.", )
tf.flags.DEFINE_string("negative_data_file", "./data/rt-polaritydata/rt-polarity.neg", "Data source for the negative data.", )
tf.flags.DEFINE_integer("batch_size", 64, "Batch Size (default: 64)", )
tf.flags.DEFINE_string("checkpoint_dir", "", "Checkpoint directory from training run", )
tf.flags.DEFINE_boolean("eval_train", False, "Evaluate on all training data", )
tf.flags.DEFINE_boolean("allow_soft_placement", True, "Allow device soft device placement", )
tf.flags.DEFINE_boolean("log_device_placement", False, "Log placement of ops on devices", )
FLAGS = tf.flags.FLAGS
FLAGS._parse_flags()
if hvd.rank() == 0:
  print("\nParameters:", )
for (attr, value) in sorted(FLAGS.__flags.items(), ):
  if hvd.rank() == 0:
    print("{}={}".format(attr.upper(), value, ), )
if hvd.rank() == 0:
  print("", )
if FLAGS.eval_train:
  (x_raw, y_test) = data_helpers.load_data_and_labels(FLAGS.positive_data_file, FLAGS.negative_data_file, )
  y_test = np.argmax(y_test, axis=1, )
else:
  x_raw = ["a masterpiece four years in the making", "everything is off."]
  y_test = [1, 0]
vocab_path = os.path.join(FLAGS.checkpoint_dir, "..", "vocab", )
vocab_processor = learn.preprocessing.VocabularyProcessor.restore(vocab_path, )
x_test = np.array(list(vocab_processor.transform(x_raw, ), ), )
if hvd.rank() == 0:
  print("\nEvaluating...\n", )
checkpoint_file = tf.train.latest_checkpoint(FLAGS.checkpoint_dir, )
graph = tf.Graph()
with graph.as_default():
  session_conf = tf.ConfigProto(allow_soft_placement=FLAGS.allow_soft_placement, log_device_placement=FLAGS.log_device_placement, )
  config.gpu_options.visible_device_list = str(hvd.local_rank(), )
  sess = tf.Session(config=session_conf, )
  with sess.as_default():
    saver = tf.train.import_meta_graph("{}.meta".format(checkpoint_file, ), )
    saver.restore(sess, checkpoint_file, )
    input_x = graph.get_operation_by_name("input_x", ).outputs[0]
    dropout_keep_prob = graph.get_operation_by_name("dropout_keep_prob", ).outputs[0]
    predictions = graph.get_operation_by_name("output/predictions", ).outputs[0]
    batches = data_helpers.batch_iter(list(x_test, ), FLAGS.batch_size, 1, shuffle=False, )
    all_predictions = []
    for x_test_batch in batches:
      batch_predictions = sess.run(predictions, {input_x: x_test_batch, dropout_keep_prob: 1.0}, )
      all_predictions = np.concatenate([all_predictions, batch_predictions], )
if y_test is not None:
  correct_predictions = float(sum(all_predictions == y_test, ), )
  if hvd.rank() == 0:
    print("Total number of test examples: {}".format(len(y_test, ), ), )
  if hvd.rank() == 0:
    print("Accuracy: {:g}".format(correct_predictions / float(len(y_test, ), ), ), )
predictions_human_readable = np.column_stack((np.array(x_raw, ), all_predictions), )
out_path = os.path.join(FLAGS.checkpoint_dir, "..", "prediction.csv", )
if hvd.rank() == 0:
  print("Saving evaluation to {0}".format(out_path, ), )
with open(out_path, "w", ) as f:
  csv.writer(f, ).writerows(predictions_human_readable, )

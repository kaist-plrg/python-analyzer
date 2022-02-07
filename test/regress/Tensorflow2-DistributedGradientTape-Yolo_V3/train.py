from absl import app, flags, logging
from absl.flags import FLAGS
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
import cv2
from tensorflow.keras.callbacks import ReduceLROnPlateau, EarlyStopping, ModelCheckpoint, TensorBoard
from yolov3_tf2.utils import freeze_all
import yolov3_tf2.dataset as dataset
from tensorflow.keras import Model
from tensorflow.keras.layers import Add, Concatenate, Conv2D, Input, Lambda, LeakyReLU, MaxPool2D, UpSampling2D, ZeroPadding2D, BatchNormalization
from tensorflow.keras.regularizers import l2
from tensorflow.keras.losses import binary_crossentropy, sparse_categorical_crossentropy
from yolov3_tf2.utils import broadcast_iou
flags.DEFINE_string("dataset", "", "path to dataset", )
flags.DEFINE_string("val_dataset", "", "path to validation dataset", )
flags.DEFINE_boolean("tiny", False, "yolov3 or yolov3-tiny", )
flags.DEFINE_string("weights", "./checkpoints/yolov3.tf", "path to weights file", )
flags.DEFINE_string("classes", "./data/coco.names", "path to classes file", )
flags.DEFINE_enum("transfer", "none", ["none", "darknet", "no_output", "frozen", "fine_tune"], "none: Training from scratch, darknet: Transfer darknet, no_output: Transfer all but output, frozen: Transfer and freeze all, fine_tune: Transfer all and freeze darknet only", )
flags.DEFINE_integer("size", 416, "image size", )
flags.DEFINE_integer("epochs", 2, "number of epochs", )
flags.DEFINE_integer("batch_size", 8, "batch size", )
flags.DEFINE_float("learning_rate", 0.001, "learning rate", )
flags.DEFINE_integer("num_classes", 80, "number of classes in the model", )
flags.DEFINE_integer("weights_num_classes", None, "specify num class for `weights` file if different, useful in transfer learning with different number of classes", )
flags.DEFINE_integer("yolo_max_boxes", 100, "maximum number of boxes per image", )
flags.DEFINE_float("yolo_iou_threshold", 0.5, "iou threshold", )
flags.DEFINE_float("yolo_score_threshold", 0.5, "score threshold", )
yolo_anchors = np.array([(10, 13), (16, 30), (33, 23), (30, 61), (62, 45), (59, 119), (116, 90), (156, 198), (373, 326)], np.float32, ) / 416
yolo_anchor_masks = np.array([[6, 7, 8], [3, 4, 5], [0, 1, 2]], )
yolo_tiny_anchors = np.array([(10, 14), (23, 27), (37, 58), (81, 82), (135, 169), (344, 319)], np.float32, ) / 416
yolo_tiny_anchor_masks = np.array([[3, 4, 5], [0, 1, 2]], )
def DarknetConv(x, filters, size, strides=1, batch_norm=True, ):
  if strides == 1:
    padding = "same"
  else:
    x = ZeroPadding2D(((1, 0), (1, 0)), )(x, )
    padding = "valid"
  x = Conv2D(filters=filters, kernel_size=size, strides=strides, padding=padding, use_bias=not batch_norm, kernel_regularizer=l2(5.0E-4, ), )(x, )
  if batch_norm:
    x = BatchNormalization()(x, )
    x = LeakyReLU(alpha=0.1, )(x, )
  return x
def DarknetResidual(x, filters, ):
  prev = x
  x = DarknetConv(x, filters // 2, 1, )
  x = DarknetConv(x, filters, 3, )
  x = Add()([prev, x], )
  return x
def DarknetBlock(x, filters, blocks, ):
  x = DarknetConv(x, filters, 3, strides=2, )
  for _ in range(blocks, ):
    x = DarknetResidual(x, filters, )
  return x
def Darknet(name=None, ):
  x = inputs = Input([None, None, 3], )
  x = DarknetConv(x, 32, 3, )
  x = DarknetBlock(x, 64, 1, )
  x = DarknetBlock(x, 128, 2, )
  x = x_36 = DarknetBlock(x, 256, 8, )
  x = x_61 = DarknetBlock(x, 512, 8, )
  x = DarknetBlock(x, 1024, 4, )
  return tf.keras.Model(inputs, (x_36, x_61, x), name=name, )
def DarknetTiny(name=None, ):
  x = inputs = Input([None, None, 3], )
  x = DarknetConv(x, 16, 3, )
  x = MaxPool2D(2, 2, "same", )(x, )
  x = DarknetConv(x, 32, 3, )
  x = MaxPool2D(2, 2, "same", )(x, )
  x = DarknetConv(x, 64, 3, )
  x = MaxPool2D(2, 2, "same", )(x, )
  x = DarknetConv(x, 128, 3, )
  x = MaxPool2D(2, 2, "same", )(x, )
  x = x_8 = DarknetConv(x, 256, 3, )
  x = MaxPool2D(2, 2, "same", )(x, )
  x = DarknetConv(x, 512, 3, )
  x = MaxPool2D(2, 1, "same", )(x, )
  x = DarknetConv(x, 1024, 3, )
  return tf.keras.Model(inputs, (x_8, x), name=name, )
def YoloConv(filters, name=None, ):
  def yolo_conv(x_in, ):
    if isinstance(x_in, tuple, ):
      inputs = (Input(x_in[0].shape[1:], ), Input(x_in[1].shape[1:], ))
      (x, x_skip) = inputs
      x = DarknetConv(x, filters, 1, )
      x = UpSampling2D(2, )(x, )
      x = Concatenate()([x, x_skip], )
    else:
      x = inputs = Input(x_in.shape[1:], )
    x = DarknetConv(x, filters, 1, )
    x = DarknetConv(x, filters * 2, 3, )
    x = DarknetConv(x, filters, 1, )
    x = DarknetConv(x, filters * 2, 3, )
    x = DarknetConv(x, filters, 1, )
    return Model(inputs, x, name=name, )(x_in, )
  return yolo_conv
def YoloConvTiny(filters, name=None, ):
  def yolo_conv(x_in, ):
    if isinstance(x_in, tuple, ):
      inputs = (Input(x_in[0].shape[1:], ), Input(x_in[1].shape[1:], ))
      (x, x_skip) = inputs
      x = DarknetConv(x, filters, 1, )
      x = UpSampling2D(2, )(x, )
      x = Concatenate()([x, x_skip], )
    else:
      x = inputs = Input(x_in.shape[1:], )
      x = DarknetConv(x, filters, 1, )
    return Model(inputs, x, name=name, )(x_in, )
  return yolo_conv
def YoloOutput(filters, anchors, classes, name=None, ):
  def yolo_output(x_in, ):
    x = inputs = Input(x_in.shape[1:], )
    x = DarknetConv(x, filters * 2, 3, )
    x = DarknetConv(x, anchors * (classes + 5), 1, batch_norm=False, )
    x = Lambda(lambda x,  : tf.reshape(x, (-1, tf.shape(x, )[1], tf.shape(x, )[2], anchors, classes + 5), ), )(x, )
    return tf.keras.Model(inputs, x, name=name, )(x_in, )
  return yolo_output
def _meshgrid(n_a, n_b, ):
  return [tf.reshape(tf.tile(tf.range(n_a, ), [n_b], ), (n_b, n_a), ), tf.reshape(tf.repeat(tf.range(n_b, ), n_a, ), (n_b, n_a), )]
def yolo_boxes(pred, anchors, classes, ):
  grid_size = tf.shape(pred, )[1:3]
  (box_xy, box_wh, objectness, class_probs) = tf.split(pred, (2, 2, 1, classes), axis=-1, )
  box_xy = tf.sigmoid(box_xy, )
  objectness = tf.sigmoid(objectness, )
  class_probs = tf.sigmoid(class_probs, )
  pred_box = tf.concat((box_xy, box_wh), axis=-1, )
  grid = _meshgrid(grid_size[1], grid_size[0], )
  grid = tf.expand_dims(tf.stack(grid, axis=-1, ), axis=2, )
  box_xy = (box_xy + tf.cast(grid, tf.float32, )) / tf.cast(grid_size, tf.float32, )
  box_wh = tf.exp(box_wh, ) * anchors
  box_x1y1 = box_xy - box_wh / 2
  box_x2y2 = box_xy + box_wh / 2
  bbox = tf.concat([box_x1y1, box_x2y2], axis=-1, )
  return (bbox, objectness, class_probs, pred_box)
def yolo_nms(outputs, anchors, masks, classes, ):
  (b, c, t) = ([], [], [])
  for o in outputs:
    b.append(tf.reshape(o[0], (tf.shape(o[0], )[0], -1, tf.shape(o[0], )[-1]), ), )
    c.append(tf.reshape(o[1], (tf.shape(o[1], )[0], -1, tf.shape(o[1], )[-1]), ), )
    t.append(tf.reshape(o[2], (tf.shape(o[2], )[0], -1, tf.shape(o[2], )[-1]), ), )
  bbox = tf.concat(b, axis=1, )
  confidence = tf.concat(c, axis=1, )
  class_probs = tf.concat(t, axis=1, )
  scores = confidence * class_probs
  dscores = tf.squeeze(scores, axis=0, )
  scores = tf.reduce_max(dscores, [1], )
  bbox = tf.reshape(bbox, (-1, 4), )
  classes = tf.argmax(dscores, 1, )
  (selected_indices, selected_scores) = tf.image.non_max_suppression_with_scores(boxes=bbox, scores=scores, max_output_size=FLAGS.yolo_max_boxes, iou_threshold=FLAGS.yolo_iou_threshold, score_threshold=FLAGS.yolo_score_threshold, soft_nms_sigma=0.5, )
  num_valid_nms_boxes = tf.shape(selected_indices, )[0]
  selected_indices = tf.concat([selected_indices, tf.zeros(FLAGS.yolo_max_boxes - num_valid_nms_boxes, tf.int32, )], 0, )
  selected_scores = tf.concat([selected_scores, tf.zeros(FLAGS.yolo_max_boxes - num_valid_nms_boxes, tf.float32, )], -1, )
  boxes = tf.gather(bbox, selected_indices, )
  boxes = tf.expand_dims(boxes, axis=0, )
  scores = selected_scores
  scores = tf.expand_dims(scores, axis=0, )
  classes = tf.gather(classes, selected_indices, )
  classes = tf.expand_dims(classes, axis=0, )
  valid_detections = num_valid_nms_boxes
  valid_detections = tf.expand_dims(valid_detections, axis=0, )
  return (boxes, scores, classes, valid_detections)
def YoloV3(size=None, channels=3, anchors=yolo_anchors, masks=yolo_anchor_masks, classes=80, training=False, ):
  x = inputs = Input([size, size, channels], name="input", )
  (x_36, x_61, x) = Darknet(name="yolo_darknet", )(x, )
  x = YoloConv(512, name="yolo_conv_0", )(x, )
  output_0 = YoloOutput(512, len(masks[0], ), classes, name="yolo_output_0", )(x, )
  x = YoloConv(256, name="yolo_conv_1", )((x, x_61), )
  output_1 = YoloOutput(256, len(masks[1], ), classes, name="yolo_output_1", )(x, )
  x = YoloConv(128, name="yolo_conv_2", )((x, x_36), )
  output_2 = YoloOutput(128, len(masks[2], ), classes, name="yolo_output_2", )(x, )
  if training:
    return Model(inputs, (output_0, output_1, output_2), name="yolov3", )
  boxes_0 = Lambda(lambda x,  : yolo_boxes(x, anchors[masks[0]], classes, ), name="yolo_boxes_0", )(output_0, )
  boxes_1 = Lambda(lambda x,  : yolo_boxes(x, anchors[masks[1]], classes, ), name="yolo_boxes_1", )(output_1, )
  boxes_2 = Lambda(lambda x,  : yolo_boxes(x, anchors[masks[2]], classes, ), name="yolo_boxes_2", )(output_2, )
  outputs = Lambda(lambda x,  : yolo_nms(x, anchors, masks, classes, ), name="yolo_nms", )((boxes_0[:3], boxes_1[:3], boxes_2[:3]), )
  return Model(inputs, outputs, name="yolov3", )
def YoloV3Tiny(size=None, channels=3, anchors=yolo_tiny_anchors, masks=yolo_tiny_anchor_masks, classes=80, training=False, ):
  x = inputs = Input([size, size, channels], name="input", )
  (x_8, x) = DarknetTiny(name="yolo_darknet", )(x, )
  x = YoloConvTiny(256, name="yolo_conv_0", )(x, )
  output_0 = YoloOutput(256, len(masks[0], ), classes, name="yolo_output_0", )(x, )
  x = YoloConvTiny(128, name="yolo_conv_1", )((x, x_8), )
  output_1 = YoloOutput(128, len(masks[1], ), classes, name="yolo_output_1", )(x, )
  if training:
    return Model(inputs, (output_0, output_1), name="yolov3", )
  boxes_0 = Lambda(lambda x,  : yolo_boxes(x, anchors[masks[0]], classes, ), name="yolo_boxes_0", )(output_0, )
  boxes_1 = Lambda(lambda x,  : yolo_boxes(x, anchors[masks[1]], classes, ), name="yolo_boxes_1", )(output_1, )
  outputs = Lambda(lambda x,  : yolo_nms(x, anchors, masks, classes, ), name="yolo_nms", )((boxes_0[:3], boxes_1[:3]), )
  return Model(inputs, outputs, name="yolov3_tiny", )
def YoloLoss(anchors, classes=80, ignore_thresh=0.5, ):
  def yolo_loss(y_true, y_pred, ):
    (pred_box, pred_obj, pred_class, pred_xywh) = yolo_boxes(y_pred, anchors, classes, )
    pred_xy = pred_xywh[..., 0:2]
    pred_wh = pred_xywh[..., 2:4]
    (true_box, true_obj, true_class_idx) = tf.split(y_true, (4, 1, 1), axis=-1, )
    true_xy = (true_box[..., 0:2] + true_box[..., 2:4]) / 2
    true_wh = true_box[..., 2:4] - true_box[..., 0:2]
    box_loss_scale = 2 - true_wh[..., 0] * true_wh[..., 1]
    grid_size = tf.shape(y_true, )[1]
    grid = tf.meshgrid(tf.range(grid_size, ), tf.range(grid_size, ), )
    grid = tf.expand_dims(tf.stack(grid, axis=-1, ), axis=2, )
    true_xy = true_xy * tf.cast(grid_size, tf.float32, ) - tf.cast(grid, tf.float32, )
    true_wh = tf.math.log(true_wh / anchors, )
    true_wh = tf.where(tf.math.is_inf(true_wh, ), tf.zeros_like(true_wh, ), true_wh, )
    obj_mask = tf.squeeze(true_obj, -1, )
    best_iou = tf.map_fn(lambda x,  : tf.reduce_max(broadcast_iou(x[0], tf.boolean_mask(x[1], tf.cast(x[2], tf.bool, ), ), ), axis=-1, ), (pred_box, true_box, obj_mask), tf.float32, )
    ignore_mask = tf.cast(best_iou < ignore_thresh, tf.float32, )
    xy_loss = obj_mask * box_loss_scale * tf.reduce_sum(tf.square(true_xy - pred_xy, ), axis=-1, )
    wh_loss = obj_mask * box_loss_scale * tf.reduce_sum(tf.square(true_wh - pred_wh, ), axis=-1, )
    obj_loss = binary_crossentropy(true_obj, pred_obj, )
    obj_loss = obj_mask * obj_loss + (1 - obj_mask) * ignore_mask * obj_loss
    class_loss = obj_mask * sparse_categorical_crossentropy(true_class_idx, pred_class, )
    xy_loss = tf.reduce_sum(xy_loss, axis=(1, 2, 3), )
    wh_loss = tf.reduce_sum(wh_loss, axis=(1, 2, 3), )
    obj_loss = tf.reduce_sum(obj_loss, axis=(1, 2, 3), )
    class_loss = tf.reduce_sum(class_loss, axis=(1, 2, 3), )
    return xy_loss + wh_loss + obj_loss + class_loss
  return yolo_loss
def main(_argv, ):
  physical_devices = tf.config.experimental.list_physical_devices("GPU", )
  for physical_device in physical_devices:
    tf.config.experimental.set_memory_growth(physical_device, True, )
  if FLAGS.tiny:
    model = YoloV3Tiny(FLAGS.size, training=True, classes=FLAGS.num_classes, )
    anchors = yolo_tiny_anchors
    anchor_masks = yolo_tiny_anchor_masks
  else:
    model = YoloV3(FLAGS.size, training=True, classes=FLAGS.num_classes, )
    anchors = yolo_anchors
    anchor_masks = yolo_anchor_masks
  if FLAGS.dataset:
    train_dataset = dataset.load_tfrecord_dataset(FLAGS.dataset, FLAGS.classes, FLAGS.size, )
  else:
    train_dataset = dataset.load_fake_dataset()
  train_dataset = train_dataset.shuffle(buffer_size=512, )
  train_dataset = train_dataset.batch(FLAGS.batch_size, )
  train_dataset = train_dataset.map(lambda x, y,  : (dataset.transform_images(x, FLAGS.size, ), dataset.transform_targets(y, anchors, anchor_masks, FLAGS.size, )), )
  train_dataset = train_dataset.prefetch(buffer_size=tf.data.experimental.AUTOTUNE, )
  if FLAGS.val_dataset:
    val_dataset = dataset.load_tfrecord_dataset(FLAGS.val_dataset, FLAGS.classes, FLAGS.size, )
  else:
    val_dataset = dataset.load_fake_dataset()
  val_dataset = val_dataset.batch(FLAGS.batch_size, )
  val_dataset = val_dataset.map(lambda x, y,  : (dataset.transform_images(x, FLAGS.size, ), dataset.transform_targets(y, anchors, anchor_masks, FLAGS.size, )), )
  if FLAGS.transfer == "none":
    pass
  elif FLAGS.transfer in ["darknet", "no_output"]:
    if FLAGS.tiny:
      model_pretrained = YoloV3Tiny(FLAGS.size, training=True, classes=FLAGS.weights_num_classes or FLAGS.num_classes, )
    else:
      model_pretrained = YoloV3(FLAGS.size, training=True, classes=FLAGS.weights_num_classes or FLAGS.num_classes, )
    if hvd.rank() == 0:
      model_pretrained.load_weights(FLAGS.weights, )
    if FLAGS.transfer == "darknet":
      model.get_layer("yolo_darknet", ).set_weights(model_pretrained.get_layer("yolo_darknet", ).get_weights(), )
      freeze_all(model.get_layer("yolo_darknet", ), )
    elif FLAGS.transfer == "no_output":
      for l in model.layers:
        if not l.name.startswith("yolo_output", ):
          l.set_weights(model_pretrained.get_layer(l.name, ).get_weights(), )
          freeze_all(l, )
  else:
    if hvd.rank() == 0:
      model.load_weights(FLAGS.weights, )
    if FLAGS.transfer == "fine_tune":
      darknet = model.get_layer("yolo_darknet", )
      freeze_all(darknet, )
    elif FLAGS.transfer == "frozen":
      freeze_all(model, )
  optimizer = tf.keras.optimizers.Adam(lr=FLAGS.learning_rate, )
  loss = [YoloLoss(anchors[mask], classes=FLAGS.num_classes, ) for mask in anchor_masks]
  avg_loss = tf.keras.metrics.Mean("loss", dtype=tf.float32, )
  avg_val_loss = tf.keras.metrics.Mean("val_loss", dtype=tf.float32, )
  for epoch in range(1, FLAGS.epochs + 1, ):
    for (batch, (images, labels)) in enumerate(train_dataset, ):
      with tf.GradientTape() as tape:
        outputs = model(images, training=True, )
        regularization_loss = tf.reduce_sum(model.losses, )
        pred_loss = []
        for (output, label, loss_fn) in zip(outputs, labels, loss, ):
          pred_loss.append(loss_fn(label, output, ), )
        total_loss = tf.reduce_sum(pred_loss, ) + regularization_loss
      tape = hvd.DistributedGradientTape(tape, )
      grads = tape.gradient(total_loss, model.trainable_variables, )
      id_new = zip(grads, model.trainable_variables, )
      optimizer.apply_gradients(id_new, )
      global hvd_broadcast_done
      if not hvd_broadcast_done:
        hvd.broadcast_variables([x[1] for x in id_new], root_rank=0, )
        hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
        hvd_broadcast_done = True
      logging.info("{}_train_{}, {}, {}".format(epoch, batch, total_loss.numpy(), list(map(lambda x,  : np.sum(x.numpy(), ), pred_loss, ), ), ), )
      avg_loss.update_state(total_loss, )
    for (batch, (images, labels)) in enumerate(val_dataset, ):
      outputs = model(images, )
      regularization_loss = tf.reduce_sum(model.losses, )
      pred_loss = []
      for (output, label, loss_fn) in zip(outputs, labels, loss, ):
        pred_loss.append(loss_fn(label, output, ), )
      total_loss = tf.reduce_sum(pred_loss, ) + regularization_loss
      logging.info("{}_val_{}, {}, {}".format(epoch, batch, total_loss.numpy(), list(map(lambda x,  : np.sum(x.numpy(), ), pred_loss, ), ), ), )
      avg_val_loss.update_state(total_loss, )
    logging.info("{}, train: {}, val: {}".format(epoch, avg_loss.result().numpy(), avg_val_loss.result().numpy(), ), )
    avg_loss.reset_states()
    avg_val_loss.reset_states()
    if hvd.rank() == 0:
      model.save_weights("checkpoints/yolov3_train_{}.tf".format(epoch, ), )
if __name__ == "__main__":
  try:
    app.run(main, )
  except SystemExit:
    pass

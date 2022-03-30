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
import config as c
from tqdm import tqdm
from tensorflow.keras import optimizers
from utils.data_utils import train_iterator
from utils.eval_utils import cross_entropy_batch, correct_num_batch, l2_loss
from model.ResNet import ResNet
from model.ResNet_v2 import ResNet_v2
from test import test
class CosineDecayWithWarmUP(tf.keras.experimental.CosineDecay, ):
  def __init__(self, initial_learning_rate, decay_steps, alpha=0.0, warm_up_step=0, name=None, ):
    self.warm_up_step = warm_up_step
    super(CosineDecayWithWarmUP, self, ).__init__(initial_learning_rate=initial_learning_rate, decay_steps=decay_steps, alpha=alpha, name=name, )
  @tf.function
  def __call__(self, step, ):
    if step <= self.warm_up_step:
      return step / self.warm_up_step * self.initial_learning_rate
    else:
      return super(CosineDecayWithWarmUP, self, ).__call__(step - self.warm_up_step, )
@tf.function
def train_step(model, images, labels, optimizer, ):
  with tf.GradientTape() as tape:
    prediction = model(images, training=True, )
    ce = cross_entropy_batch(labels, prediction, label_smoothing=c.label_smoothing, )
    l2 = l2_loss(model, )
    loss = ce + l2
    gradients = tape.gradient(loss, model.trainable_variables, )
  tape = hvd.DistributedGradientTape(tape, )
  id_new = zip(gradients, model.trainable_variables, )
  optimizer.apply_gradients(id_new, )
  global hvd_broadcast_done
  if not hvd_broadcast_done:
    hvd.broadcast_variables([x[1] for x in id_new], root_rank=0, )
    hvd.broadcast_variables(optimizer.variables(), root_rank=0, )
    hvd_broadcast_done = True
  return (ce, prediction)
def train(model, data_iterator, optimizer, log_file, ):
  sum_ce = 0
  sum_correct_num = 0
  for i in range(c.iterations_per_epoch // hvd.size(), ):
    (images, labels) = data_iterator.next()
    (ce, prediction) = train_step(model, images, labels, optimizer, )
    correct_num = correct_num_batch(labels, prediction, )
    sum_ce += ce * c.batch_size
    sum_correct_num += correct_num
    if hvd.rank() == 0:
      print("ce: {:.4f}, accuracy: {:.4f}, l2 loss: {:.4f}".format(ce, correct_num / c.batch_size, l2_loss(model, ), ), )
  if hvd.rank() == 0:
    log_file.write("train: cross entropy loss: {:.4f}, l2 loss: {:.4f}, accuracy: {:.4f}\n".format(sum_ce / c.train_num, l2_loss(model, ), sum_correct_num / c.train_num, ), )
if __name__ == "__main__":
  physical_devices = tf.config.experimental.list_physical_devices("GPU", )
  tf.config.experimental.set_memory_growth(device=physical_devices[0], enable=True, )
  train_data_iterator = train_iterator()
  model = ResNet_v2(50, )
  model.build(input_shape=(None,) + c.input_shape, )
  if hvd.rank() == 0:
    model.summary()
  if hvd.rank() == 0:
    print("initial l2 loss:{:.4f}".format(l2_loss(model, ), ), )
  if c.load_weight_file is not None:
    if hvd.rank() == 0:
      model.load_weights(c.load_weight_file, )
    if hvd.rank() == 0:
      print("pretrain weight l2 loss:{:.4f}".format(l2_loss(model, ), ), )
  learning_rate_schedules = CosineDecayWithWarmUP(initial_learning_rate=c.initial_learning_rate * hvd.size(), decay_steps=c.epoch_num * c.iterations_per_epoch - c.warm_iterations, alpha=c.minimum_learning_rate, warm_up_step=c.warm_iterations, )
  optimizer = optimizers.SGD(learning_rate=learning_rate_schedules, momentum=0.9, )
  for epoch_num in range(c.epoch_num, ):
    with open(c.log_file, "a", ) as f:
      if hvd.rank() == 0:
        f.write("epoch:{}\n".format(epoch_num, ), )
      train(model, train_data_iterator, optimizer, f, )
      test(model, f, )
    if hvd.rank() == 0:
      model.save_weights(c.save_weight_file, save_format="h5", )
    if epoch_num % 5 == 4:
      os.system("cp {} {}_epoch_{}.h5".format(c.save_weight_file, c.save_weight_file.split(".", )[0], epoch_num, ), )

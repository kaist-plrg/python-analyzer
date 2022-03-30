import os
import numpy as np
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
from gan import Generator, Discriminator
import datetime
current_time = datetime.datetime.now().strftime("%Y%m%d-%H%M%S", )
train_log_dir = "logs/org-board/" + current_time + "/train"
train_summary_writer = tf.summary.create_file_writer(train_log_dir, )
def save_result(val_out, val_block_size, image_fn, color_mode, ):
  def preprocess(img, ):
    img = ((img + 1.0) * 127.5).astype(np.uint8, )
    return img
  preprocesed = preprocess(val_out, )
  final_image = np.array([], )
  single_row = np.array([], )
  for b in range(val_out.shape[0], ):
    if single_row.size == 0:
      single_row = preprocesed[b, :, :, :]
    else:
      single_row = np.concatenate((single_row, preprocesed[b, :, :, :]), axis=1, )
    if (b + 1) % val_block_size == 0:
      if final_image.size == 0:
        final_image = single_row
      else:
        final_image = np.concatenate((final_image, single_row), axis=0, )
      single_row = np.array([], )
  if final_image.shape[2] == 1:
    final_image = np.squeeze(final_image, axis=2, )
def celoss_ones(logits, smooth=0.0, ):
  return tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(logits=logits, labels=tf.ones_like(logits, ) * (1.0 - smooth), ), )
def celoss_zeros(logits, smooth=0.0, ):
  return tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(logits=logits, labels=tf.zeros_like(logits, ) * (1.0 - smooth), ), )
def d_loss_fn(generator, discriminator, input_noise, real_image, is_trainig, ):
  fake_image = generator(input_noise, is_trainig, )
  d_real_logits = discriminator(real_image, is_trainig, )
  d_fake_logits = discriminator(fake_image, is_trainig, )
  d_loss_real = celoss_ones(d_real_logits, smooth=0.1, )
  d_loss_fake = celoss_zeros(d_fake_logits, smooth=0.0, )
  loss = d_loss_real + d_loss_fake
  return loss
def g_loss_fn(generator, discriminator, input_noise, is_trainig, ):
  fake_image = generator(input_noise, is_trainig, )
  d_fake_logits = discriminator(fake_image, is_trainig, )
  loss = celoss_ones(d_fake_logits, smooth=0.1, )
  return loss
def main():
  tf.random.set_seed(22, )
  np.random.seed(22, )
  os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
  assert tf.__version__.startswith("2.", )
  z_dim = 100
  epochs = 300000
  batch_size = 128
  learning_rate = 2.0E-4
  is_training = True
  assets_dir = "./images"
  if not os.path.isdir(assets_dir, ):
    os.makedirs(assets_dir, )
  val_block_size = 10
  val_size = val_block_size * val_block_size
  ((x_train, _), (x_test, _)) = keras.datasets.mnist.load_data()
  x_train = x_train.astype(np.float32, ) / 255.0
  db = tf.data.Dataset.from_tensor_slices(x_train, ).shuffle(batch_size * 4, ).batch(batch_size, ).repeat()
  db_iter = iter(db, )
  inputs_shape = [-1, 28, 28, 1]
  generator = Generator()
  generator.build(input_shape=(batch_size, z_dim), )
  if hvd.rank() == 0:
    generator.summary()
  discriminator = Discriminator()
  discriminator.build(input_shape=(batch_size, 28, 28, 1), )
  if hvd.rank() == 0:
    discriminator.summary()
  d_optimizer = keras.optimizers.Adam(learning_rate=learning_rate * hvd.size(), beta_1=0.5, )
  g_optimizer = keras.optimizers.Adam(learning_rate=learning_rate * hvd.size(), beta_1=0.5, )
  for epoch in range(epochs, ):
    batch_x = next(db_iter, )
    batch_x = tf.reshape(batch_x, shape=inputs_shape, )
    batch_x = batch_x * 2.0 - 1.0
    batch_z = tf.random.uniform(shape=[batch_size, z_dim], minval=-1.0, maxval=1.0, )
    with tf.GradientTape() as tape:
      d_loss = d_loss_fn(generator, discriminator, batch_z, batch_x, is_training, )
    tape = hvd.DistributedGradientTape(tape, )
    grads = tape.gradient(d_loss, discriminator.trainable_variables, )
    d_optimizer.apply_gradients(zip(grads, discriminator.trainable_variables, ), )
    global hvd_broadcast_done
    if not hvd_broadcast_done:
      hvd.broadcast_variables(discriminator.variables, root_rank=0, )
      hvd.broadcast_variables(d_optimizer.variables(), root_rank=0, )
      hvd_broadcast_done = True
    with train_summary_writer.as_default():
      tf.summary.scalar("d_loss", d_loss, step=epoch, )
    with tf.GradientTape() as tape:
      g_loss = g_loss_fn(generator, discriminator, batch_z, is_training, )
    tape = hvd.DistributedGradientTape(tape, )
    grads = tape.gradient(g_loss, generator.trainable_variables, )
    g_optimizer.apply_gradients(zip(grads, generator.trainable_variables, ), )
    global hvd_broadcast_done
    if not hvd_broadcast_done:
      hvd.broadcast_variables(generator.variables, root_rank=0, )
      hvd.broadcast_variables(g_optimizer.variables(), root_rank=0, )
      hvd_broadcast_done = True
    if epoch % 100 == 0:
      if hvd.rank() == 0:
        print(epoch, "d loss:", float(d_loss, ), "g loss:", float(g_loss, ), )
      """
            # validation results at every epoch            val_z = np.random.uniform(-1, 1, size=(val_size, z_dim))            fake_image = generator(val_z, training=False)            image_fn = os.path.join('images', 'gan-val-{:03d}.png'.format(epoch + 1))            save_result(fake_image.numpy(), val_block_size, image_fn, color_mode='L')            """
if __name__ == "__main__":
  main()

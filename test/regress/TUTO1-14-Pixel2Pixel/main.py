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
import numpy as np
from tensorflow import keras
import time
from matplotlib import pyplot as plt
from gd import Discriminator, Generator
tf.random.set_seed(22, )
np.random.seed(22, )
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
assert tf.__version__.startswith("2.", )
batch_size = 1
IMG_WIDTH = 256
IMG_HEIGHT = 256
path_to_zip = keras.utils.get_file("facades.tar.gz", cache_subdir=os.path.abspath(".", ), origin="https://people.eecs.berkeley.edu/~tinghuiz/projects/pix2pix/datasets/facades.tar.gz", extract=True, )
PATH = os.path.join(os.path.dirname(path_to_zip, ), "facades/", )
if hvd.rank() == 0:
  print("dataset path:", PATH, )
def load_image(image_file, is_train, ):
  """
    load and preprocess images    :param image_file:    :param is_train:    :return:    """
  image = tf.io.read_file(image_file, )
  image = tf.image.decode_jpeg(image, )
  w = image.shape[1]
  w = w // 2
  real_image = image[:, :w, :]
  input_image = image[:, w:, :]
  input_image = tf.cast(input_image, tf.float32, )
  real_image = tf.cast(real_image, tf.float32, )
  if is_train:
    input_image = tf.image.resize(input_image, [286, 286], )
    real_image = tf.image.resize(real_image, [286, 286], )
    stacked_image = tf.stack([input_image, real_image], axis=0, )
    cropped_image = tf.image.random_crop(stacked_image, size=[2, IMG_HEIGHT, IMG_WIDTH, 3], )
    (input_image, real_image) = (cropped_image[0], cropped_image[1])
    if np.random.random() > 0.5:
      input_image = tf.image.flip_left_right(input_image, )
      real_image = tf.image.flip_left_right(real_image, )
  else:
    input_image = tf.image.resize(input_image, size=[IMG_HEIGHT, IMG_WIDTH], )
    real_image = tf.image.resize(real_image, size=[IMG_HEIGHT, IMG_WIDTH], )
  input_image = input_image / 127.5 - 1
  real_image = real_image / 127.5 - 1
  out = tf.concat([input_image, real_image], axis=2, )
  return out
train_dataset = tf.data.Dataset.list_files(PATH + "/train/*.jpg", )
train_iter = iter(train_dataset, )
train_data = []
for x in train_iter:
  train_data.append(load_image(x, True, ), )
train_data = tf.stack(train_data, axis=0, )
if hvd.rank() == 0:
  print("train:", train_data.shape, )
train_dataset = tf.data.Dataset.from_tensor_slices(train_data, )
train_dataset = train_dataset.shuffle(400, ).batch(1, )
test_dataset = tf.data.Dataset.list_files(PATH + "test/*.jpg", )
test_iter = iter(test_dataset, )
test_data = []
for x in test_iter:
  test_data.append(load_image(x, False, ), )
test_data = tf.stack(test_data, axis=0, )
if hvd.rank() == 0:
  print("test:", test_data.shape, )
test_dataset = tf.data.Dataset.from_tensor_slices(test_data, )
test_dataset = test_dataset.shuffle(400, ).batch(1, )
generator = Generator()
generator.build(input_shape=(batch_size, 256, 256, 3), )
if hvd.rank() == 0:
  generator.summary()
discriminator = Discriminator()
discriminator.build(input_shape=[(batch_size, 256, 256, 3), (batch_size, 256, 256, 3)], )
if hvd.rank() == 0:
  discriminator.summary()
g_optimizer = keras.optimizers.Adam(learning_rate=2.0E-4 * hvd.size(), beta_1=0.5, )
d_optimizer = keras.optimizers.Adam(learning_rate=2.0E-4 * hvd.size(), beta_1=0.5, )
def discriminator_loss(disc_real_output, disc_generated_output, ):
  real_loss = keras.losses.binary_crossentropy(tf.ones_like(disc_real_output, ), disc_real_output, from_logits=True, )
  generated_loss = keras.losses.binary_crossentropy(tf.zeros_like(disc_generated_output, ), disc_generated_output, from_logits=True, )
  real_loss = tf.reduce_mean(real_loss, )
  generated_loss = tf.reduce_mean(generated_loss, )
  total_disc_loss = real_loss + generated_loss
  return total_disc_loss
def generator_loss(disc_generated_output, gen_output, target, ):
  LAMBDA = 100
  gan_loss = keras.losses.binary_crossentropy(tf.ones_like(disc_generated_output, ), disc_generated_output, from_logits=True, )
  l1_loss = tf.reduce_mean(tf.abs(target - gen_output, ), )
  gan_loss = tf.reduce_mean(gan_loss, )
  total_gen_loss = gan_loss + LAMBDA * l1_loss
  return total_gen_loss
def generate_images(model, test_input, tar, epoch, ):
  prediction = model(test_input, training=True, )
  plt.figure(figsize=(15, 15), )
  display_list = [test_input[0], tar[0], prediction[0]]
  title = ["Input Image", "Ground Truth", "Predicted Image"]
  for i in range(3, ):
    plt.subplot(1, 3, i + 1, )
    plt.title(title[i], )
    plt.imshow(display_list[i] * 0.5 + 0.5, )
    plt.axis("off", )
  plt.savefig("images/epoch%d.png" % epoch, )
  if hvd.rank() == 0:
    print("saved images.", )
def main():
  epochs = 1000
  for epoch in range(epochs, ):
    start = time.time()
    for (step, inputs) in enumerate(train_dataset, ):
      (input_image, target) = tf.split(inputs, num_or_size_splits=[3, 3], axis=3, )
      with tf.GradientTape() as gen_tape, tf.GradientTape() as disc_tape:
        gen_output = generator(input_image, training=True, )
        disc_real_output = discriminator([input_image, target], training=True, )
        disc_generated_output = discriminator([input_image, gen_output], training=True, )
        gen_loss = generator_loss(disc_generated_output, gen_output, target, )
        disc_loss = discriminator_loss(disc_real_output, disc_generated_output, )
      gen_tape = hvd.DistributedGradientTape(gen_tape, )
      generator_gradients = gen_tape.gradient(gen_loss, generator.trainable_variables, )
      g_optimizer.apply_gradients(zip(generator_gradients, generator.trainable_variables, ), )
      global hvd_broadcast_done
      if not hvd_broadcast_done:
        hvd.broadcast_variables(generator.variables, root_rank=0, )
        hvd.broadcast_variables(g_optimizer.variables(), root_rank=0, )
        hvd_broadcast_done = True
      discriminator_gradients = disc_tape.gradient(disc_loss, discriminator.trainable_variables, )
      d_optimizer.apply_gradients(zip(discriminator_gradients, discriminator.trainable_variables, ), )
      global hvd_broadcast_done
      if not hvd_broadcast_done:
        hvd.broadcast_variables(discriminator.variables, root_rank=0, )
        hvd.broadcast_variables(d_optimizer.variables(), root_rank=0, )
        hvd_broadcast_done = True
      if step % 100 == 0:
        if hvd.rank() == 0:
          print(epoch, step, float(disc_loss, ), float(gen_loss, ), )
    if epoch % 1 == 0:
      for inputs in test_dataset:
        (input_image, target) = tf.split(inputs, num_or_size_splits=[3, 3], axis=3, )
        generate_images(generator, input_image, target, epoch, )
        break
    if hvd.rank() == 0:
      print("Time taken for epoch {} is {} sec\n".format(epoch + 1, time.time() - start, ), )
  for inputs in test_dataset:
    (input_image, target) = tf.split(inputs, num_or_size_splits=[3, 3], axis=3, )
    generate_images(generator, input_image, target, 99999, )
    break
if __name__ == "__main__":
  main()

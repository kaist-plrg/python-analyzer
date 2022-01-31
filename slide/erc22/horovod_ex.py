import tensorflow as tf
import horovod.tensorflow as hvd
hvd.init() # Init Horovod.

mnist_model = tf.keras.Sequential([...])
loss = tf.losses.SparseCategoricalCrossentropy()
opt = tf.optimizers.Adam(0.001 * hvd.size())  # Adjust lr based on # GPUs

@tf.function
def training_step(images, labels, first_batch):
    # Forward propagation
    with tf.GradientTape() as tape:
        probs = mnist_model(images, training=True)
        loss_value = loss(labels, probs)

    # Horovod: add Horovod Distributed GradientTape.
    tape = hvd.DistributedGradientTape(tape)
    grads = tape.gradient(loss_value, mnist_model.trainable_variables)
    opt.apply_gradients(zip(grads, mnist_model.trainable_variables))

    # Horovod: broadcast initial variable states
    if first_batch:
        hvd.broadcast_variables(mnist_model.variables, root_rank=0)
        hvd.broadcast_variables(opt.variables(), root_rank=0)

    return loss_value

# Horovod: adjust number of steps based on number of GPUs.
for batch, (images, labels) in enumerate(dataset.take(10000 // hvd.size())):
    loss_value = training_step(images, labels, batch == 0)

    if batch % 10 == 0 and hvd.local_rank() == 0:
        print('Step #%d\tLoss: %.6f' % (batch, loss_value))

# Horovod: save checkpoints only on worker 0
if hvd.rank() == 0: checkpoint.save(checkpoint_dir)

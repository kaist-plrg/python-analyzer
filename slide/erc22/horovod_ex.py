import tensorflow as tf
# Import and Initialize Horovod
import horovod.tensorflow as hvd
hvd.init()

mnist_model = tf.keras.Sequential([...])
loss = tf.losses.SparseCategoricalCrossentropy()
# Scale the Learning Rate
opt = tf.optimizers.Adam(0.001 * hvd.size())

for images, labels in dataset.take(1000//hvd.size()):
    with tf.GradientTape() as tape:
        probs = mnist_model(images, training=True)
        loss_value = loss(labels, probs)

    # Wrapping DistributedGradientTape
    tape = hvd.DistributedGradientTape(tape)
    grads = tape.gradient(loss_value, 
            mnist_model.trainable_variables)
    opt.apply_gradients(zip(grads, 
         mnist_model.trainable_variables)) 

    # Root Rank Blocking
    if hvd.local_rank() == 0:
        print('Loss: %.6f' % (loss_value))

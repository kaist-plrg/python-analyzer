# train_tape.py
for data in dataset.take(1000 // hvd.size()):
    with tf.GradientTape() as tape:
        probs = model(data[0])
        loss_value = loss(data[1], probs)

    tape = hvd.DistributedGradientTape(tape)
    grads = tape.gradient(...)


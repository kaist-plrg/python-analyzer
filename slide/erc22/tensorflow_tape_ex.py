# train_tape.py
for data in dataset.take(1000):
    with tf.GradientTape() as tape:
        probs = model(data[0])
        loss_value = loss(data[1], probs)

    grads = tape.gradient(...)

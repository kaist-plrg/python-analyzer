# using low-level API and loop
for batch_x, batch_y in train_data.take(training_steps):
    # Run the optimization to update W and b values.
    with tf.GradientTape() as tape:
        pred = model(x, is_training=True)
        loss = loss_compute(y, pred)

    trainable_vars = model.trainable_variables
    gradients = tape.gradient(loss, trainable_vars)
    optimizer.apply_gradients(zip(gradients, trainable_vars)) 

# using model.fit
model.compile(optimizer = optimizer, loss = loss_compute) 
model.fit(train_data.take(training_steps))

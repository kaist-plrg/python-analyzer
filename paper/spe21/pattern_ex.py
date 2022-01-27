# training_steps : number of steps for feeding training data
# model : tf.Model instance
# train_data : tf.data.Dataset instance
CCE = keras.losses.CategoricalCrossentropy()
ADAM = tf.optimizers.Adam()

# using low-level API and loop
for batch_x, batch_y in train_data.take(training_steps):
    # Run the optimization to update W and b values.
    with tf.GradientTape() as tape:
        pred = model(x, is_training=True)
        loss = CCE(y, pred)

    trainable_vars = model.trainable_variables
    gradients = tape.gradient(loss, trainable_vars)
    ADAM.apply_gradients(zip(gradients, trainable_vars)) 

# using model.fit
model.compile(optimizer = ADAM, loss = CCE) 
model.fit(train_data.take(training_steps))

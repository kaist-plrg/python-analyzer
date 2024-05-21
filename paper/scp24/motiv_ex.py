import tensorflow as tf
import tensorflow.keras as keras

class ResNet(keras.models.Model):
    def __init__(self, layer_num):
        pass

model = ResNet(18)
optim = keras.optimizers.Adam(learning_rate=0.001)
model.compile(optimizer = optim)
model.fit(train_dataset)

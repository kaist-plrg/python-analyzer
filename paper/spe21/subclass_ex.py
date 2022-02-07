import tensorflow as tf
import tensorflow.keras as keras

class ResNet(keras.models.Model): 
    def __init__(self, layer_num, **kwargs):
        pass
    def call(self, inputs):
        pass

if __name__ = '___main__':
    model = ResNet(50)
    model.build(input_shape=(None,) + c.input_shape) 
    optim = tf.keras.optimizers.SGD()
    model.fit(optimizer=optim, train_data) 

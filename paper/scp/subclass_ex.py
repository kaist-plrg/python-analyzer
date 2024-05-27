import tensorflow as tf
import tensorflow.keras as keras

# user-defined training class
class ResNet(keras.models.Model): 
    def __init__(self, layer_num, **kwargs):
        pass
    def call(self, inputs):
        pass

# arbitrary non-training class 
class Something():
    def fit(optimizer, y):
        print("hello, world!") 

if __name__ = '___main__': 
    # actual DL model instance
    model = ResNet(50)
    # object not related to training
    something = Something()

    model.build(input_shape=(None,) + c.input_shape) 
    optim = tf.keras.optimizers.SGD()

    # training-related method call, which should be transformed
    model.fit(optimizer=optim, train_data) 
    # method call not related to the training  
    something.fit(optimizer=optim, train_data)

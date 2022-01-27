# /model/ResNet.py
import tensorflow as tf

class ResNet(tf.keras.models.Model): 
    def __init__(self, layer_num, **kwargs):
        ...


# /train.py
import tensorflow as tf
from model.ResNet import ResNet

# ...

if __name__ = '___main__':
    model = ResNet(50)
    model.build(input_shape=(None,) + c.input_shape) 
    optim = tf.keras.optimizers.SGD()
    model.fit(optimizer=optim, train_data) 

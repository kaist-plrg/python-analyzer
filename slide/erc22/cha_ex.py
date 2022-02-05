class MyModel(keras.models.Model):
        pass

model = MyModel()
model.compile(optimizer=Adam(0.001))
model.fit(dataset)

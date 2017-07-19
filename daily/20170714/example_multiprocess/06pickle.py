class O:
    def __init__(self, x):
        self.x = x

        def f():
            return self.x

        self.f = f


import pickle
print(pickle.dumps(O("x")))

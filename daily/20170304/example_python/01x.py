class X(object):
    def __init__(self, d):
        self.x = foo(d)
        self.y = boo(d)
        raise RuntimeError("hmm")


x = X.__new__(X)
x.__dict__.update({"y": 10, "z": 20})
print(x, x.y, x.z)
# <__main__.X object at 0x10f7e4f28> 10 20

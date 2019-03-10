class X:
    def __init__(self):
        self.a = "aaa"

    @property
    def b(self):
        return self.a

    @b.setter
    def b(self, v):
        self.a = v


x = X()
print(x.a, x.b)
x.a = "bbb"
print(x.a, x.b)
x.b = "ccc"
print(x.a, x.b)
# -- stdout --------------------
# >> aaa aaa
# >> bbb bbb
# >> ccc ccc

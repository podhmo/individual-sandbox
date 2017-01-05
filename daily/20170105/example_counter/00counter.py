from collections import Counter


class Pair(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Pair(self.x + other.x, self.y + other.y)

    def __repr__(self):
        return "<P {self.x}, {self.y}>".format(self=self)

c0 = Counter({"foo": Pair(10, 20)})
c1 = Counter({"foo": Pair(10, 20)})
print(c0, c1)
print(c0 + c1) # error

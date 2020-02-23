class EnumerableMixin:
    def map(self, fn):
        return [fn(x) for x in self.each()]


class List(EnumerableMixin):
    def __init__(self, xs):
        self.xs = xs

    def each(self):
        return iter(self.xs)


print(List([10, 20, 30]).map(lambda x: x * x))
# [100, 400, 900]

class FakeArray:
    def __init__(self, xs, *, shape=None):
        self.xs = xs
        self.shape = shape or len(xs)

    def reshape(self, shape):
        return FakeArray(_reshape(self.flatten(), shape))

    def flatten(self, xs=None):
        for x in xs or self.xs:
            if isinstance(x, (list, tuple)):
                yield from self.flatten(x)
            else:
                yield x

    def __repr__(self):
        from io import StringIO

        o = StringIO()
        padding = " " * len("array([")
        for i, x in enumerate(self.xs):
            if i == 0:
                o.write("array([")
            else:
                o.write(",\n")
                o.write(padding)
            o.write(repr(x))
        o.write("])")
        return o.getvalue()


def arange(i, j):
    return FakeArray(list(range(i, j)))


def _reshape(itr, units):
    if not isinstance(units, (list, tuple)):  # integer?
        return [next(itr) for _ in range(units)]
    elif len(units) == 1:
        return [next(itr) for _ in range(units[0])]
    else:
        return [_reshape(itr, units[1:]) for _ in range(units[0])]


print(arange(0, 9))
print(arange(0, 9).reshape((3, 3)))
print(arange(0, 27).reshape((3, 3, 3)))

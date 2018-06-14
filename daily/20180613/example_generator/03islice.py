import itertools


class W:
    def __init__(self, xs):
        self.xs = xs

    def __getitem__(self, k):
        if isinstance(k, slice):
            return itertools.islice(self.xs, k.start, k.stop, k.step)
        else:
            return itertools.islice(self.xs, k)


def gen():
    for i in range(10):
        print(i)
        yield i


xs = W(gen())
for i in xs[2:5]:
    print("@", i)

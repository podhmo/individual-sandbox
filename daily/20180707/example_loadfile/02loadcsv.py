import csv
import sys


class Wrap:
    def __init__(self, target):
        self.target = target

    def __getattr__(self, name):
        return getattr(self, name)

    def __iter__(self):
        return WrapIterator(self.target)


class WrapIterator:
    def __init__(self, target, *, teardown=None):
        self.target = target
        self.itr = iter(target)
        self.teardown = teardown

    def __next__(self):
        try:
            return next(self.itr)
        except StopIteration as e:
            if self.teardown is not None:
                self.teardown()
                self.teardown = None
            raise

    def __iter__(self):
        return self


def loadfile(f=None):
    if f == None:
        return Wrap(csv.DictReader(sys.stdin))
    else:
        return Wrap(csv.DictReader(open(f)))


# error
xs = loadfile("data.csv")
print(list(xs))

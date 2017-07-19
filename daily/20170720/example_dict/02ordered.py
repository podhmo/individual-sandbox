from collections import OrderedDict, Counter


# from: https://docs.python.org/3/library/collections.html
class OrderedCounter(Counter, OrderedDict):
    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, OrderedDict(self))

    # for pickle
    def __reduce__(self):
        return self.__class__, (OrderedDict(self), )


it = iter([2, 1, 3, 3, 3, 2])

c = OrderedCounter(it)
print(c)
print(c.most_common())

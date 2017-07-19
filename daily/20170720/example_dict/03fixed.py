from collections import Mapping, namedtuple


def fixed(name, field):
    T = namedtuple(name, field)

    class D(Mapping):
        __slots__ = ("data", )

        def __init__(self, *args, **kwargs):
            self.data = T(*args, **kwargs)

        def __len__(self):
            return len(self.data)

        def __getitem__(self, k):
            try:
                return getattr(self.data, k)
            except AttributeError:
                raise KeyError(k)

        def __iter__(self):
            return iter(self.data._fields)

        def __contains__(self, key):
            return hasattr(self.data, key)

        def __repr__(self):
            return repr(self.data)

    D.__name__ = name
    return D


class Hand:
    __slots__ = tuple("n{}".format(i) for i in range(10))

    def __init__(self, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9):
        self.n0 = n0
        self.n1 = n1
        self.n2 = n2
        self.n3 = n3
        self.n4 = n4
        self.n5 = n5
        self.n6 = n6
        self.n7 = n7
        self.n8 = n8
        self.n9 = n9

    def __getattr__(self, k):
        return getattr(self.__dict__, k)


Point = fixed("Point", "x y z")
pt = Point(x=10, y=20, z=30)
print(pt["x"])
print(list(pt))
print(list(pt.items()))

import matplotlib.pyplot as plt
from pympler.asizeof import asizeof
plt.style.use("ggplot")

named = {n: fixed("N{}".format(n), " ".join("n{}".format(i) for i in range(n))) for n in range(20)}

ns = list(range(11))
lists = [asizeof(list(range(i))) for i in ns]
dicts = [asizeof({i: i for i in range(n)}) for n in ns]
tuples = [asizeof(tuple(range(i))) for i in ns]
nameds = [asizeof(named[i](*range(i))) for i in ns]

lratios = [x / y for x, y in zip(lists, tuples)]
dratios = [x / y for x, y in zip(dicts, tuples)]
nratios = [x / y for x, y in zip(nameds, tuples)]
plt.plot(ns, lratios, "r", label="list")
plt.plot(ns, dratios, "g", label="dict")
plt.plot(ns, nratios, "b", label="named")
plt.plot(ns, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, asizeof(Hand(*range(10)))], "k", label="hand")
plt.legend()
plt.show()

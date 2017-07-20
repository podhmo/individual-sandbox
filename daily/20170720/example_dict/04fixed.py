# https://github.com/podhmo/minicollections
from collections import Mapping, namedtuple
from minicollections import valueobject
from pympler.asizeof import asizeof


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


from graphlib import draw  # NOQA
with draw() as plt:
    N = 11
    named_map = {
        n: namedtuple("N{}".format(n), " ".join("n{}".format(i) for i in range(n)))
        for n in range(N)
    }
    fixed_map = {
        n: fixed("N{}".format(n), " ".join("n{}".format(i) for i in range(n)))
        for n in range(N)
    }
    object_map = {
        n: valueobject("V{}".format(n), [("n{}".format(i), int, 0) for i in range(n)])
        for n in range(N)
    }

    ns = list(range(N))
    lists = [asizeof(list(range(i))) for i in ns]
    dicts = [asizeof({i: i for i in range(n)}) for n in ns]
    tuples = [asizeof(tuple(range(i))) for i in ns]
    nameds = [asizeof(named_map[i](*range(i))) for i in ns]
    fixeds = [asizeof(fixed_map[i](*range(i))) for i in ns]
    objects = [asizeof(object_map[i](*range(i))) for i in ns]

    lratios = [x / y for x, y in zip(lists, tuples)]
    dratios = [x / y for x, y in zip(dicts, tuples)]
    nratios = [x / y for x, y in zip(nameds, tuples)]
    fratios = [x / y for x, y in zip(fixeds, tuples)]
    oratios = [x / y for x, y in zip(objects, tuples)]

    plt.plot(ns, lratios, "r", label="list")
    plt.plot(ns, dratios, "g", label="dict")
    plt.plot(ns, nratios, "b", label="namedtuple")
    plt.plot(ns, oratios, "k", label="class with slots")
    plt.plot(ns, fratios, "m", label="fixed")

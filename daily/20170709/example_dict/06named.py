# import matplotlib
# matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
from pympler.asizeof import asizeof
plt.style.use("ggplot")

from collections import namedtuple
named = {
    n: namedtuple("N{}".format(n), " ".join("n{}".format(i) for i in range(n)))
    for n in range(100)
}

ns = list(range(20))
lists = [asizeof(list(range(i))) for i in ns]
dicts = [asizeof({i: i for i in range(n)}) for n in ns]
tuples = [asizeof(tuple(range(i))) for i in ns]
nameds = [asizeof(named[i](*range(i))) for i in ns]
sdicts = [asizeof({"name{}".format(i): i for i in range(n)}) for n in ns]
sdicts2 = [asizeof({"namenamename{}".format(i): i for i in range(n)}) for n in ns]

lratios = [x / y for x, y in zip(lists, tuples)]
dratios = [x / y for x, y in zip(dicts, tuples)]
nratios = [x / y for x, y in zip(nameds, tuples)]
sdratios = [x / y for x, y in zip(sdicts, tuples)]
sdratios2 = [x / y for x, y in zip(sdicts2, tuples)]
plt.plot(ns, lratios, "r", ns, dratios, "g", ns, nratios, "b", ns, sdratios, "k", ns, sdratios2, "m")
plt.show()

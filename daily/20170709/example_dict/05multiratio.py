# import matplotlib
# matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
from pympler.asizeof import asizeof
plt.style.use("ggplot")

ns = list(range(10))
lists = [asizeof(list(range(i))) for i in ns]
dicts = [asizeof({i: i for i in range(n)}) for n in ns]
tuples = [asizeof(tuple(range(i))) for i in ns]

lratios = [x / y for x, y in zip(lists, tuples)]
dratios = [x / y for x, y in zip(dicts, tuples)]
plt.plot(ns, lratios, "r", ns, dratios, "g")
plt.show()

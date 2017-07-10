# import matplotlib
# matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
import array
from pympler.asizeof import asizeof
plt.style.use("ggplot")

ns = list(range(50))
lists = [asizeof(list(range(i))) for i in ns]
arrays = [asizeof(array.array("i", range(i))) for i in ns]
tuples = [asizeof(tuple(range(i))) for i in ns]

lratios = [x / y for x, y in zip(lists, tuples)]
aratios = [x / y for x, y in zip(arrays, tuples)]
plt.plot(ns, lratios, "b", ns, aratios, "g")
plt.show()

# import matplotlib
# matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
from pympler.asizeof import asizeof
plt.style.use("ggplot")

ns = [10 ** n for n in range(6)]
lists = [asizeof(list(range(i))) for i in ns]
dicts = [asizeof({i: i for i in range(n)}) for n in ns]
tuples = [asizeof(tuple(range(i))) for i in ns]
plt.plot(ns, lists, "r", ns, dicts, "g", ns, tuples, "b")
plt.show()

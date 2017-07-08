# import matplotlib
# matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
from pympler.asizeof import asizeof
plt.style.use("ggplot")

xs = [asizeof({i: i for i in range(n)}) for n in range(100)]
plt.plot(xs)
plt.show()

import matplotlib
matplotlib.use("AGG")  # NOQA
import matplotlib.pyplot as plt
import numpy as np
plt.style.use("ggplot")

t = np.arange(0.0, 2.0, 0.01)
s = 1 + np.sin(2 * np.pi * t)
plt.plot(t, s)
plt.xlabel('time (s)')
plt.ylabel('voltage (mV)')
plt.title('About as simple as it gets, folks')
plt.grid(True)

dpi = 96.0
plt.gcf().set_size_inches(300 / dpi, 200 / dpi)
plt.savefig("images/test-ggplot.png", dpi=dpi)

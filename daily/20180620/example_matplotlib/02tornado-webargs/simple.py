import numpy as np
import matplotlib as mlp
mlp.use("webagg")  # noqa
import matplotlib.pyplot as plt

fig = plt.figure(1)
a = fig.add_subplot(111)
t = np.arange(0.0, 3.0, 0.01)
s = np.sin(2 * np.pi * t)
a.plot(t, s)
plt.show()

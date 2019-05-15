# pip install mplcursors

import matplotlib.pyplot as plt
import numpy as np
import mplcursors

np.random.seed(42)

fig, ax = plt.subplots()
ax.scatter(*np.random.random((2, 26)))
ax.set_title("Mouse over a point")

mplcursors.cursor(hover=True)

plt.show()

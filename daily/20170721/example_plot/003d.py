import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


def func(x, y):
    return x ** 2 + y ** 2


x = np.arange(-5, 5, 0.05)
y = np.arange(-5, 5, 0.05)

X, Y = np.meshgrid(x, y)

Z = func(X, Y)

fig = plt.figure()
ax = Axes3D(fig)
ax.plot_surface(X, Y, Z)
plt.show()

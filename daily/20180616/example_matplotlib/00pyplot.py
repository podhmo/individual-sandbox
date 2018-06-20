# https://matplotlib.org/tutorials/introductory/pyplot.html
import numpy as np
import matplotlib.pyplot as plt


def f(t):
    return np.exp(-t) * np.cos(2 * np.pi * t)


t1 = np.arange(0.0, 5.0, 0.1)
t2 = np.arange(0.0, 5.0, 0.02)

fig = plt.figure(1)
plt.plot(t1, f(t1), 'bo', t2, f(t2), 'k')
plt.show()

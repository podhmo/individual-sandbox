import numpy as np
import matplotlib.pyplot as plt

"""
複数の図を表示する
"""


def f(t):
    # e^-t * cos(2πt)
    return np.exp(-t) * np.cos(2 * np.pi * t)


t1 = np.arange(0.0, 5.0, 0.1)
t2 = np.arange(0.0, 5.0, 0.02)

plt.figure(1)

plt.subplot(311)
plt.grid(True)
plt.plot(t1, f(t1), 'bo', t2, f(t2), 'k')

plt.subplot(312)
plt.plot(t2, np.cos(2 * np.pi * t2), 'r--')

plt.subplot(313)
plt.plot(t2, np.exp(-t2), 'g--')

plt.show()

import numpy as np
import matplotlib.pyplot as plt

"""
numpyのarrayを使う
"""

t = np.arange(0., 5, 0.2)
print(t)

plt.plot(t, t, 'r--', t, t ** 2, 'bs', t, t ** 3, 'g^')
plt.show()

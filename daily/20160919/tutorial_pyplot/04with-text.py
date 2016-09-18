import numpy as np
import matplotlib.pyplot as plt

"""
テキストの表示
"""

mu, sigma = 100, 15
x = mu + sigma * np.random.randn(10000)

n, bins, patches = plt.hist(x, 50, normed=1, facecolor="g", alpha=0.75)


plt.xlabel('Smarts')
plt.ylabel('Probability')
plt.title('Histgram of IQ')
plt.text(60, 0.25, r'$\mu=100,\ \sigma=15$')
plt.axis([40, 160, 0, 0.03])
plt.grid(True)

plt.show()

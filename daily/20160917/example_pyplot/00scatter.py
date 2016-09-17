import numpy as np
import matplotlib.pyplot as plt

# generate data
x1 = np.random.rand(100) * 0.5
y1 = np.random.rand(100)

x2 = np.random.rand(100) * 0.5 + 0.5
y2 = np.random.rand(100)

fig, ax = plt.subplots(1, 1)

ax.scatter(x1, y1, c='red')
ax.scatter(x2, y2, c='blue')

ax.set_title('second scatter plot')
ax.set_xlabel('x')
ax.set_ylabel('y')

plt.show()

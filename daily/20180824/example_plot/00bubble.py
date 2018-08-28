# libraries
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns  # noqa

# create data
x = np.random.rand(15)
y = x + np.random.rand(15)
z = x + np.random.rand(15)
z = z * z

# Change color with c and alpha. I map the color to the X axis value.
plt.scatter(x, y, s=z * 2000, c=x, cmap="Blues", alpha=0.4, edgecolors="grey", linewidth=2)

# Add titles (main and on axis)
plt.xlabel("the X axis")
plt.ylabel("the Y axis")
plt.title("A colored bubble plot")

plt.show()

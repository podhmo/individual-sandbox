#!/usr/bin/env python
# coding: utf-8

# In[1]:


import sys
import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

print('**', matplotlib.rcParams['backend'])
x = np.linspace(0, 2, 100)

plt.figure(figsize=(10, 8))

plt.plot(x, x, label="linear")
plt.plot(x, x ** 2, label="quadratic")
plt.plot(x, x ** 3, label="cubic")

plt.xlabel("x label")
plt.ylabel("y label")

plt.title("Simple Plot")

plt.legend()

filename = os.environ.get("FILENAME", "sample_plt.png")
print(f"save {filename}", file=sys.stderr)
plt.savefig(filename)


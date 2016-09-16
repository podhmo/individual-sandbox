# -*- coding:utf-8 -*-
import numpy as np
a = np.array([0, 1, 2, 3, 4])
print(a)
b = np.roll(a, 1)
print(b)
c = np.roll(a, -1)
print(c)
c[-1] = 9
print(c)

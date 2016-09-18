# -*- coding:utf-8 -*-

"""
X軸Y軸の範囲を指定
"""
import matplotlib.pyplot as plt


xs = [1, 2, 3, 4]
ys = [x * x for x in xs]
plt.plot(xs, ys, 'ro')  # 'ro'
plt.axis([0, 6, 0, 20])
plt.show()

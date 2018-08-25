import numpy as np

a = [np.array([1, 2]), np.array([3, 4])]
c = [np.array([1, 2]), np.array([3, 4])]
print([a, c])
for b, d in zip(a, c):
    b *= -1
print([a, c])

a = [np.array([1, 2]), np.array([3, 4])]
c = [np.array([1, 2]), np.array([3, 4])]
print([a, c])
for b in zip(a):
    b *= -1
print([a, c])

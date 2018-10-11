import numpy as np

np.random.seed(1)
N = 20
xs = np.random.randn(20)
print(np.exp(np.abs(xs) * 10).astype(dtype="int64"))

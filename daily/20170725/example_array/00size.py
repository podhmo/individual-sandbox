import numpy as np
from pympler.asizeof import asizeof  # pip install pympler
L = list(range(10))
arr = np.arange(10, dtype=np.int64)

print(L)
print(arr)
print(asizeof(L))  # 512
print(asizeof(arr))  # 176

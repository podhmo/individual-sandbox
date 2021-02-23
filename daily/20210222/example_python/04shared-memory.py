import numpy as np
from multiprocessing import shared_memory

# 使いたいのはshareableList?
a = np.array([1, 1, 2, 3, 5, 8])  # Start with an existing NumPy array
shm = shared_memory.SharedMemory(create=True, size=10)

print(shm)
b = np.ndarray(a.shape, dtype=a.dtype, buffer=shm.buf)
print(a, b)
b[:] = a[:]  # copy
print(a, b)

shm2 = shared_memory.SharedMemory(name=shm.name)
c = np.ndarray((6,), dtype=np.int64, buffer=shm2.buf)
c[0] = 100
print(a, b, c)
shm2.close()
shm.close()
shm.unlink()

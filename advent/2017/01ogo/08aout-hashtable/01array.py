import psutil
import random

p = psutil.Process()
st = p.memory_info().rss
L = [0 for i in range(1000000)]
for i in range(100):
    k = random.randint(1, 1000000)
    v = i
    L[k] = v
print("{} -> {}".format(st, p.memory_info().rss))

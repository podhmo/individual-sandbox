import psutil
from hashtable import make, assign, iterate, get
import random

p = psutil.Process()
st = p.memory_info().rss
D = make()
for i in range(100):
    k = random.randint(1, 1000000)
    v = i
    assign(D, k, v)

k0, v0 = next(iter(iterate(D)))
print(get(D, k0))
print("{} -> {}".format(st, p.memory_info().rss))

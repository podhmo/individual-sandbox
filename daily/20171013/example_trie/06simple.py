import string
import random


@profile
def gen(cs, N, size):
    d = {}
    for i in range(N):
        k = "".join([random.choice(cs) for _ in range(size)])
        v = "".join([random.choice(cs) for _ in range(size)])
        d[k] = v
    return d

@profile
def main():
    size = 50
    N = 10000
    t = gen(string.printable, N, size)
    import gc
    gc.collect()
    import time
    time.sleep(1)
    import psutil
    print(psutil.Process().memory_info())


main()

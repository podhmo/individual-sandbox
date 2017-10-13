import string
import pygtrie
import numpy.random


@profile
def gen(cs, N, size):
    words = numpy.random.choice(cs, [N, size])
    t = pygtrie.StringTrie()
    for word in words:
        t[str(word)] = str(word)
    return t


@profile
def main():
    cs = numpy.array(list(str(string.printable)), dtype="|S1")
    size = 50
    N = 10000
    t = gen(cs, N, size)
    import gc
    gc.collect()
    import time
    time.sleep(1)
    import psutil
    print(psutil.Process().memory_info())


main()

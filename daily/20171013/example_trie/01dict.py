import string
import numpy.random

cs = numpy.array(list(str(string.printable)), dtype="|S1")
size = 50
N = 100000


def gen():
    words = numpy.random.choice(cs, [N, size])
    d = {}
    for word in words:
        d[str(word)] = str(word)
    return d


d = gen()
import psutil
print(psutil.Process().memory_info())


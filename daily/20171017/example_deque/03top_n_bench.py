import timeit


n = 100
r = timeit.repeat(
    """\
xs = r.randint(1, 100000, size=10000)
top200 = collect_topn(xs, 200)
""",
    number=n,
    setup="""\
import numpy.random as r
from collections import deque

def collect_topn(xs, n):
    itr = iter(xs)
    min_item = next(itr)
    buf = deque([min_item], maxlen=n)
    min_k = (buf[-1])

    for item in itr:
        k = (item)
        if len(buf) < n or k >= min_k:
            found = False
            for i, x in enumerate(buf):
                if k >= (x):
                    if len(buf) >= n:
                        buf.pop()
                    buf.insert(i, item)
                    found = True
                    break
            if not found:
                buf.append(item)
            min_k = (buf[-1])
    return list(buf)
"""
)
print(r)

r = timeit.repeat(
    """\
xs = r.randint(1, 100000, size=10000)
top20 = collect_topn(xs, 20)
""",
    number=n,
    setup="""\
import numpy.random as r

def collect_topn(xs, n):
    itr = iter(xs)
    min_item = next(itr)
    buf = [min_item]
    min_k = (buf[-1])

    for item in itr:
        k = (item)
        if len(buf) < n or k >= min_k:
            found = False
            for i, x in enumerate(buf):
                if k >= (x):
                    if len(buf) >= n:
                        buf.pop()
                    buf.insert(i, item)
                    found = True
                    break
            if not found:
                buf.append(item)
            min_k = (buf[-1])
    return list(buf)
"""
)
print(r)

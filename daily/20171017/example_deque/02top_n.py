import timeit
import numpy.random as r
from collections import deque


def collect_topn(xs, n):
    itr = iter(xs)
    min_item = next(itr)
    buf = deque([min_item], maxlen=n)
    min_k = (buf[-1])

    # almost insertion sort
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


def collect_topn2(xs, n):
    itr = iter(xs)
    min_item = next(itr)
    buf = deque([min_item], maxlen=n)
    min_k = (buf[-1])

    # almost insertion sort
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


xs = r.randint(1, 100000, size=1000)
print(collect_topn(xs, 20) == collect_topn2(xs, 20))
top20 = collect_topn(xs, 20)
print(top20)
print(collect_topn(xs, 200) == collect_topn2(xs, 200))
top20 = collect_topn(xs, 200)
print(top20)

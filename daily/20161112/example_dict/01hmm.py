from collections import Counter
import copy
import json


class A:
    pass


def gen():
    return dict(ID=A(), Name=A(), Age=A())


def f(d):
    for k in d.keys():
        d[k.lower()] = d.pop(k)
    return d

c = Counter()
N = 1000

for _ in range(N):
    d = gen()
    d["X"] = A()
    c[json.dumps(list(sorted(f(d).keys())))] += 1

print(c)

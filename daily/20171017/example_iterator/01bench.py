import timeit

N = 1000
M = 100
r = timeit.timeit(
    """\
xs = list(range(5))
deque(alternately(*[rep(xs, 100) for i in range(100)]), maxlen=0)
""",
    number=N,
    setup="""\
from collections import deque

def rep(xs, n):
    for i in range(n):
        yield i, xs

def alternately(*xs):
    itrs = [iter(x) for x in xs]
    while itrs:
        poped = []
        for itr in itrs:
            try:
                yield next(itr)
            except StopIteration:
                poped.append(itr)
        if poped:
            itrs = [itr for itr in itrs if itr not in poped]
"""
)
print("alternately")
print(r)

r = timeit.timeit(
    """\
xs = list(range(5))
deque(roundrobin(*[rep(xs, 100) for i in range(100)]), maxlen=0)
""",
    number=N,
    setup="""\
from collections import deque
from itertools import cycle, islice

def rep(xs, n):
    for i in range(n):
        yield i, xs

def roundrobin(*iterables):
    "roundrobin('ABC', 'D', 'EF') --> A D E B F C"
    # Recipe credited to George Sakkis
    pending = len(iterables)
    nexts = cycle(iter(it).__next__ for it in iterables)
    while pending:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            pending -= 1
            nexts = cycle(islice(nexts, pending))
"""
)

print("roundrobin")
print(r)

r = timeit.timeit(
    """\
xs = list(range(5))
deque(chain.from_iterable(zip_longest(*[rep(xs, 100) for i in range(100)])), maxlen=0)
""",
    number=N,
    setup="""\
from collections import deque
from itertools import chain, zip_longest

def rep(xs, n):
    for i in range(n):
        yield i, xs
"""
)
print("zip")
print(r)

# alternately
# 1.7596824840002228
# roundrobin
# 1.3739034849859308
# zip
# 0.6299366969906259


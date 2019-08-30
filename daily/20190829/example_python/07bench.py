import gc
import time
import contextlib


@contextlib.contextmanager
def timeit(fmt: str):
    t0 = time.time()
    yield
    print(fmt.format(time.time() - t0))


N = 5000
L = [list(range(i)) for i in range(N)]
L2 = [set(range(i)) for i in range(N)]


gc.disable()
for i in range(2):
    assert not gc.isenabled()

    print("-")
    with timeit("sorted(xs): {0}"):
        [sorted(xs) for xs in L]

    with timeit("tuple(sorted(xs)): {0}"):
        [tuple(sorted(xs)) for xs in L]

    with timeit("frozenset(xs): {0}"):
        [frozenset(xs) for xs in L]

    with timeit("frozenset(xs) from set: {0}"):
        [frozenset(xs) for xs in L2]

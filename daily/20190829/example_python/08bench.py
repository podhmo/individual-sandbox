import gc
import time
import contextlib


@contextlib.contextmanager
def timeit(fmt: str):
    t0 = time.time()
    yield
    print(fmt.format(time.time() - t0))


N = 5000

L1 = [tuple(sorted(range(i))) for i in range(N)]
D1 = {k: True for k in L1}

L2 = [frozenset(range(i)) for i in range(N)]
D2 = {k: True for k in L2}


gc.disable()
for i in range(2):
    assert not gc.isenabled()

    print("-")
    with timeit("tuple(sorted(xs)): {0}"):
        [D1[k] for k in L1]

    with timeit("frozenset(xs): {0}"):
        [D2[k] for k in L2]

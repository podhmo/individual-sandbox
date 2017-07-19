import numpy as np
import time
from numba import jit
from concurrent.futures import ThreadPoolExecutor, as_completed


def loop_python(xs, size):
    s = 0
    for i in range(size):
        s += xs[i]
    return s


@jit(nopython=True, nogil=True)
def loop(xs, size):
    s = 0
    for i in range(size):
        s += xs[i]
    return s


def main():
    st = time.time()
    with ThreadPoolExecutor() as executor:
        xs = [list(range(1000))] * 10
        futs = []
        for x in xs:
            futs.append(executor.submit(loop, x, len(x)))
        for f in as_completed(futs):
            print(f.result())
    print("----------------------------------------")
    print("time:", time.time() - st)

    st = time.time()
    with ThreadPoolExecutor() as executor:
        xs = [list(range(1000))] * 10
        futs = []
        for x in xs:
            futs.append(executor.submit(loop_python, x, len(x)))
        for f in as_completed(futs):
            print(f.result())
    print("----------------------------------------")
    print("time:", time.time() - st)

    st = time.time()
    with ThreadPoolExecutor() as executor:
        xs = [list(range(1000))] * 10
        futs = []
        for x in xs:
            futs.append(executor.submit(sum, x))
        for f in as_completed(futs):
            print(f.result())
    print("----------------------------------------")
    print("time:", time.time() - st)

    st = time.time()
    with ThreadPoolExecutor() as executor:
        xs = [np.arange(1000)] * 10
        futs = []
        for x in xs:
            futs.append(executor.submit(np.sum, x))
        for f in as_completed(futs):
            print(f.result())
    print("----------------------------------------")
    print("time:", time.time() - st)


if __name__ == "__main__":
    main()

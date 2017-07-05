from magicalimport import import_symbol
import time
import random
import numpy as np


def perf(ll, n, m, name):
    f = import_symbol(name)
    elapsed = []
    # print("before: ", ll[-1])
    ans = None

    for l in ll:
        start = time.time()
        ans = f(l, n, m)
        end = time.time()
        elapsed.append(end - start)
    # print("after: ", ans)
    print(name, np.average(elapsed))


def main(*, times, size):
    random.seed(123)

    n = (70, 70)
    m = (True, True)

    ll = [
        [(random.randint(0, 99), random.randint(0, 99)) for _ in range(size)] for _ in range(times)
    ]

    perf(ll, n, m, "./00filter.py:filter")
    perf(ll, n, m, "./02simplified.py:filter")
    perf(ll, n, m, "./03break.py:filter")
    perf(ll, n, m, "./04zip.py:filter")
    perf(ll, n, m, "./05more.py:filter")
    perf(ll, n, m, "./06shortcode.py:filter")
    perf(ll, n, m, "./00filter.py:filter")
    perf(ll, n, m, "./02simplified.py:filter")
    perf(ll, n, m, "./03break.py:filter")
    perf(ll, n, m, "./04zip.py:filter")
    perf(ll, n, m, "./05more.py:filter")
    perf(ll, n, m, "./06shortcode.py:filter")


if __name__ == "__main__":
    main(times=100, size=1000)
    # e.g. times=1, size=10, n=(70, 70)
    # before:  [(68, 58), (92, 36), (18, 14), (98, 28), (74, 60), (89, 41), (9, 26), (47, 85), (92, 2), (26, 38)]
    # after:  [(92, 36), (98, 28), (74, 60), (89, 41), (47, 85), (92, 2)]

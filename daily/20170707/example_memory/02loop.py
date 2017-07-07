import threading
import time
import tracemalloc

L = []


def loop(*, size, times):
    for i in range(times):
        g(size)
        time.sleep(0.5)


def g(size):
    L.append([_ for _ in range(size)])


def take_snapshot():
    snapshot = tracemalloc.take_snapshot()
    return snapshot.filter_traces(
        (
            tracemalloc.Filter(False, "<frozen importlib._bootstrap>"),
            tracemalloc.Filter(False, "tracemalloc"),
            tracemalloc.Filter(False, "<unknown>"),
        )
    )


def peek():
    prev_snapshot = None
    snapshot = take_snapshot()
    while True:
        prev_snapshot, snapshot = snapshot, take_snapshot()
        stats = snapshot.compare_to(prev_snapshot, "lineno", cumulative=True)
        for i, stat in enumerate(stats[:5]):
            print(stat)
        time.sleep(0.5)
        print("----------------------------------------")


# tracemalloc.start()
# t = threading.Thread(daemon=True, target=peek)
# t.start()

# 40Mb位？
# 28  * 10000 * 100
loop(size=10000, times=100)

# 0: 5.1Mb
# 0.5Mb * 100 = 55Mb
# 44.8Mb, 912
# L = [[_ for _ in range(10000)] for i in range(100)]
# tracemalloc._format_size(sys.getsizeof(L) + sum(sys.getsizeof(x) for x in L) + sum(sys.getsizeof(x) for xs in L for x in xs), False)
# '35.1 MiB'
# 

# >>> [t._format_size(x, False) for x in t.get_traced_memory()]
# ['34.4 MiB', '34.4 MiB']
# >>> t._format_size(t.get_tracemalloc_memory(), False)
# '53.2 MiB'

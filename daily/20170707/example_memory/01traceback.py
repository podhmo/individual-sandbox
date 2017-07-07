import tracemalloc


# key_type = filename | lineno | traceback
def display_top(snapshot, key_type='traceback', limit=10):
    snapshot = snapshot.filter_traces(
        (
            tracemalloc.Filter(False, "<frozen importlib._bootstrap>"),
            tracemalloc.Filter(False, "<unknown>"),
        )
    )
    top_stats = snapshot.statistics(key_type)

    for index, stat in enumerate(top_stats[:limit], 1):
        print(stat)
        for line in stat.traceback.format():
            print(line)
        print("----------------------------------------")


tracemalloc.start()

L = []


def f(*, size, times):
    for i in range(times):
        g(size)


def g(size):
    L.append([_ for _ in range(size)])


f(size=100, times=10)
snapshot = tracemalloc.take_snapshot()
display_top(snapshot)

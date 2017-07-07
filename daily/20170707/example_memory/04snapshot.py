import tracemalloc as t

print("*start")
print([t._format_size(x, False) for x in t.get_traced_memory()])
t.start()

L = []


def loop(*, size, times):
    for i in range(times):
        print(len(L))
        g(size)
        print([t._format_size(x, False) for x in t.get_traced_memory()])
        snapshot = t.take_snapshot().filter_traces(
            (
                t.Filter(False, "<frozen importlib._bootstrap>"),
                t.Filter(False, "*tracemalloc*"),
                t.Filter(False, "*linecache*"),
                t.Filter(False, "*sre_*"),
                t.Filter(False, "*re.py"),
                t.Filter(False, "*fnmatch*"),
                t.Filter(False, "*tokenize*"),
                t.Filter(False, "<unknown>"),
            )
        )

        for stat in snapshot.statistics("lineno", cumulative=False)[:3]:
            print("----------------------------------------")
            print(t._format_size(stat.size, False))
            for line in stat.traceback.format():
                print(line)
        print("========================================")


def g(size):
    L.append([_ for _ in range(size)])
    # import time
    # time.sleep(0.2)


loop(size=10000, times=100)
t.stop()
print("*end")
print([t._format_size(x, False) for x in t.get_traced_memory()])

import tracemalloc as t

print("*start")
print([t._format_size(x, False) for x in t.get_traced_memory()])
t.start()

L = [[_ for _ in range(10000)] for i in range(100)]
print("*gen")
print([t._format_size(x, False) for x in t.get_traced_memory()])

snapshot = t.take_snapshot()
for stats in snapshot.statistics("traceback")[:3]:
    print(stats)

print("----------------------------------------")
snapshot = t.take_snapshot()
for stats in snapshot.statistics("lineno", cumulative=True)[:3]:
    print(stats)

t.stop()
print([t._format_size(x, False) for x in t.get_traced_memory()])

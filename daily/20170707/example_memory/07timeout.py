import tracemalloc as t
import threading
import logging
logger = logging.getLogger(__name__)
L = []


def loop(*, size, times):
    for i in range(times):
        logger.info(
            "memory (current, peak) %s",
            str([t._format_size(x, False) for x in t.get_traced_memory()])
        )
        g(size)
        # time.sleep(0.2)


def g(size):
    L.append([_ for _ in range(size)])


def display():
    # snapshot = t.take_snapshot().filter_traces(
    #     (
    #         t.Filter(False, "<frozen importlib._bootstrap>"),
    #         t.Filter(False, "<unknown>"),
    #     )
    # )
    snapshot = t.take_snapshot()
    for stat in snapshot.statistics("lineno", cumulative=False)[:5]:
        print("----------------------------------------")
        print(t._format_size(stat.size, False))
        for line in stat.traceback.format():
            print(line)
    print("========================================")


if __name__ == '__main__':
    t.start()
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
    th = threading.Thread(target=loop, kwargs=dict(size=10000, times=100), daemon=True)
    th.start()
    th.join(timeout=2.0)
    display()

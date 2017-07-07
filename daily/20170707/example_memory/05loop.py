# python -X tracemalloc <thisfile>
import tracemalloc as t
import time
import threading
import logging
logger = logging.getLogger(__name__)
L = []


def tick():
    while True:
        logger.info("%s", str([t._format_size(x, False) for x in t.get_traced_memory()]))
        time.sleep(0.2)


def loop(*, size, times):
    for i in range(times):
        logger.info("len %s", len(L))
        g(size)
        time.sleep(0.2)


def g(size):
    L.append([_ for _ in range(size)])


logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
t.start()
th = threading.Thread(daemon=True, target=tick)
th.start()
loop(size=10000, times=100)

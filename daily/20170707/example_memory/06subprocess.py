import tracemalloc as t
import sys
import os
import signal
import logging
import time
logger = logging.getLogger(__name__)
L = []


def loop(*, size, times):
    for i in range(times):
        logger.info("len %s", len(L))
        g(size)
        time.sleep(0.2)


def g(size):
    L.append([_ for _ in range(size)])


def handle_traceback(sig, frame):
    logger.info(
        "memory (current, peak) %s", str([t._format_size(x, False) for x in t.get_traced_memory()])
    )
    import traceback
    traceback.print_stack(limit=5)


def tick(pid):
    while True:
        print("kill", pid)
        os.kill(pid, signal.SIGUSR1)
        time.sleep(0.2)


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
    print("parent", str(os.getpid()))
    pid = os.getpid()
    from multiprocessing import Process
    p = Process(target=lambda: tick(pid))
    p.start()
    print("child", p.pid)
    signal.signal(signal.SIGUSR1, handle_traceback)
    t.start()
    loop(size=10000, times=10)
    p.terminate()

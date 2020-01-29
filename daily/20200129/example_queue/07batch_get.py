import typing as t
import threading
import time
import queue
import logging
from functools import partial
import handofcats

logger = logging.getLogger(__name__)


class BatchQueue:
    def __init__(self, q, *, default_timeout: float, buffer_size: int = 10):
        self.q = q
        self.buffer_size = buffer_size
        self.default_timeout = default_timeout

    def get(self, timeout: t.Optional[t.Union[int, float]] = None):
        timeout = timeout or self.default_timeout
        endtime = time.time() + timeout
        r = []
        logger.info("timeuot=%s, endtime=%s, r=%s", timeout, endtime, r)
        while True:
            v = self.q.get(timeout)
            r.append(v)
            if v is None:
                break
            if len(r) >= self.buffer_size:
                break
            timeout = endtime - time.time()
            logger.debug("timeuot=%s, endtime=%s, r=%s", timeout, endtime, r)
            if timeout < 0:
                break
        return r

    def task_done(self):
        self.q.task_done()

    def empty(self):
        self.q.empty()

    def join(self):
        self.q.join()


def producer(q, ev):
    for i in range(10):
        q.put(i)
        ev.set()
        time.sleep(0.2)
    q.put(None)


def consumer(bq):
    while True:
        items = bq.get(0.5)
        time.sleep(0.1)

        if not items:
            continue

        print(items)

        for e in items:
            bq.task_done()
        if items[-1] is None:
            break


@handofcats.as_command
def run():
    q = queue.Queue()
    ev = threading.Event()
    threading.Thread(target=partial(producer, q, ev)).start()
    threading.Thread(
        target=partial(consumer, BatchQueue(q, buffer_size=3, default_timeout=1))
    ).start()
    ev.wait()
    q.join()

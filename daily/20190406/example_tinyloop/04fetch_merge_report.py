import logging
import time
import string
import queue
from concurrent.futures import ThreadPoolExecutor

logger = logging.getLogger(__name__)


class Dispatcher:
    def __init__(self, q: queue.Queue):
        self.q = q
        self.buf = []
        self.started = time.time()

    def dispatch(self, fn, v):
        if fn == do_fetch:
            r = fn(v, q=self.q)
            return do_merge, r
        elif fn == do_merge:
            if not self.buf:
                self.q.put_nowait((do_wait, do_merge))

            self.buf.append(v)
            # todo: lock?
            if (time.time() - self.started) > 1:
                buf = self.buf
                self.buf = []
                self.started = time.time()

                r = fn(buf, q=self.q)
                return do_report, r
            else:
                return None, None
        elif fn == do_report:
            fn(v, q=self.q)
            return None, None
        elif fn == do_wait:
            cont = fn(v, q=self.q)
            return cont, ""
        else:
            raise Exception(f"unpexpected action: {fn.__name__!r}")


def worker(dispatcher):
    q = dispatcher.q
    while not q.empty():
        try:
            fn, item = q.get()
            if fn is None:
                break
            cont, item = dispatcher.dispatch(fn, item)
            q.task_done()
            if cont is None:
                continue
            q.put((cont, item))
        except Exception as e:
            print("!!", e)
            logger.exception(e)
            raise


def do_fetch(item, *, q):
    logger.info(f"fetch start %s", item)
    time.sleep(0.2)
    logger.info(f"fetch end   %s", item)
    return item


def do_merge(items, *, q):
    logger.info(f"merge start %s", items)
    time.sleep(0.7)
    logger.info(f"merge end   %s", items)
    return items


def do_report(items, *, q):
    print("REPORT", ", ".join(items))


def do_wait(cont, *, q):
    print("wait")
    time.sleep(1)
    return cont


def source():
    return list(string.ascii_uppercase)


logging.basicConfig(level=logging.INFO, format="%(asctime)s " + logging.BASIC_FORMAT)
num_worker_threads = 3
q = queue.Queue()
dispatcher = Dispatcher(q)

for item in source():
    q.put_nowait((do_fetch, item))
with ThreadPoolExecutor() as ex:
    for i in range(num_worker_threads):
        ex.submit(worker, dispatcher)
print(q.qsize())
# 漏れもあるかも？

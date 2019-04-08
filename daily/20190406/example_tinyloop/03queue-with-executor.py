import logging
import time
import string
import queue
from concurrent.futures import ThreadPoolExecutor

logger = logging.getLogger(__name__)


def worker(q):
    while not q.empty():
        item = q.get()
        do_work(item, q=q)
        q.task_done()


def do_work(item, *, q):
    logger.info(f"start %s", item)
    time.sleep(0.2)
    logger.info(f"end   %s", item)
    if item > "X":
        q.put(chr(ord(item) - 1))


def source():
    return list(string.ascii_uppercase)


logging.basicConfig(level=logging.INFO, format="%(asctime)s " + logging.BASIC_FORMAT)
num_worker_threads = 10
q = queue.Queue()

for item in source():
    q.put_nowait(item)
with ThreadPoolExecutor() as ex:
    for i in range(num_worker_threads):
        ex.submit(worker, q)
print(q.qsize())
# 漏れもあるかも？

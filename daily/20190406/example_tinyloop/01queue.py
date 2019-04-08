import logging
import time
import string
import threading
import queue

logger = logging.getLogger(__name__)


def worker():
    while True:
        item = q.get()
        if item is None:
            break
        do_work(item)
        q.task_done()


def do_work(item):
    logger.info(f"start %s", item)
    time.sleep(0.2)
    logger.info(f"end   %s", item)


def source():
    return list(string.ascii_uppercase)


logging.basicConfig(level=logging.INFO, format="%(asctime)s " + logging.BASIC_FORMAT)
num_worker_threads = 10
q = queue.Queue()
threads = []
for i in range(num_worker_threads):
    t = threading.Thread(target=worker)
    t.start()
    threads.append(t)

for item in source():
    q.put(item)

# block until all tasks are done
q.join()

# stop workers
for i in range(num_worker_threads):
    q.put(None)
for t in threads:
    t.join()

import logging
import time
import threading
import queue

from handofcats import as_command
from minitask.worker.namedpipeworker import Manager

logger = logging.getLogger(__name__)


def produce(m: Manager, uid):
    with m.open_writer_queue(uid) as q:
        for i in range(5):
            logger.info("-> %d", i)
            q.put(i)
            time.sleep(0.3)
        q.put(None)


def consume(m: Manager, uid):
    q = queue.Queue()

    def vaccum():
        with m.open_reader_queue(uid) as rq:
            for i in rq:
                q.put(i)
                logger.info("<- %d", i)
            q.put(None)
            print("OK")

    th = threading.Thread(target=vaccum)
    th.start()

    while True:
        i = q.get()
        if i is None:
            q.task_done()
            break
        logger.info("!! %d", i)
        time.sleep(1)
        q.task_done()
    th.join()
    print("ok")


@as_command
def run():
    with Manager() as m:
        uid = m.generate_uid()
        m.spawn(produce, uid=uid)
        m.spawn(consume, uid=uid)

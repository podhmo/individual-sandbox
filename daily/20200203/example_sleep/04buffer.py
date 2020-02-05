import logging
from handofcats import as_command
from minitask.worker.namedpipeworker import Manager
import time
import threading
import queue
from _hibernation import sleep

logger = logging.getLogger(__name__)


def produce(m: Manager, uid):
    with m.open_writer_queue(uid) as q:
        for i in range(5):
            logger.info("-> %d", i)
            q.put(i)
            time.sleep(0.3)
        q.put(None)


def buffer(m: Manager, uid):
    uid0, uid1 = uid.split("@", 1)
    q = queue.Queue()

    def vaccum():
        with m.open_reader_queue(uid0) as rq:
            for i in rq:
                q.put(i)
                logger.info("<= %d", i)
            q.put(None)
        print("OK")

    th = threading.Thread(target=vaccum)
    th.start()

    with m.open_writer_queue(uid1) as wq:
        while True:
            i = q.get()
            if i is None:
                q.task_done()
                break
            logger.info("=> %d", i)
            wq.put(i)
            q.task_done()
        wq.put(None)
    th.join()


def consume(m: Manager, uid):
    q = queue.Queue()

    def vaccum():
        with m.open_reader_queue(uid) as rq:
            for i in rq:
                q.put(i)
                logger.info("<- %d", i)
            q.put(None)

    th = threading.Thread(target=vaccum)
    th.start()

    while True:
        i = q.get()
        if i is None:
            q.task_done()
            break
        logger.info("!! %d", i)
        sleep(">_<", 1, verbose=False)
        q.task_done()
    th.join()
    print("ok")


@as_command
def run():
    with Manager() as m:
        uid0 = m.generate_uid("x")
        uid1 = m.generate_uid("y")
        m.spawn(produce, uid=uid0)
        m.spawn(buffer, uid=f"{uid0}@{uid1}")
        m.spawn(consume, uid=uid1)

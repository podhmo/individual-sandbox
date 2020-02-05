import logging
import time

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
    from _hibernation import sleep

    with m.open_reader_queue(uid) as q:
        for i in q:
            logger.info("!! %d", i)
            # time.sleep(1)
            sleep("", 1, verbose=False)
    print("ok")


@as_command
def run():
    with Manager() as m:
        uid = m.generate_uid()
        m.spawn(produce, uid=uid)
        m.spawn(consume, uid=uid)

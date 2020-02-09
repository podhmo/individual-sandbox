import time
import logging
import threading

from minitask.worker.namedpipeworker import Manager
from _hibernation import sleep
from handofcats import as_command

logger = logging.getLogger(__name__)


def consume(m: Manager, *, uid1: str, uid2: str) -> None:
    with m.open_writer_queue(uid2) as wq:
        with m.open_reader_queue(uid1) as q:
            for x in q:
                logger.info("<- %d", x)
                sleep("", 1, verbose=False)
                wq.put(x)


@as_command
def run() -> None:
    with Manager() as m:
        uid1 = m.generate_uid("x.in")
        uid2 = m.generate_uid("x.out")
        m.spawn(consume, uid1=uid1, uid2=uid2)

        def receive():
            with m.open_reader_queue(uid2) as q:
                for x in q:
                    time.sleep(0.5)
                    logger.info("got, %d", x)

        threading.Thread(target=receive).start()

        with m.open_writer_queue(uid1) as q:
            for x in range(7):
                logger.info("-> %d", x)
                q.put(x)
                time.sleep(0.5)

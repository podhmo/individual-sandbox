import time
import logging
from minitask.worker.namedpipeworker import Manager
from _hibernation import sleep
from handofcats import as_command

logger = logging.getLogger(__name__)


def consume(m: Manager, uid: str) -> None:
    with m.open_reader_queue(uid) as q:
        for x in q:
            logger.info("<- %d", x)
            sleep("", 1, verbose=False)


@as_command
def run() -> None:
    with Manager() as m:
        uid = m.generate_uid()
        m.spawn(consume, uid=uid)

        with m.open_writer_queue(uid) as q:
            for x in range(7):
                logger.info("-> %d", x)
                q.put(x)
                time.sleep(0.5)

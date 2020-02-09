import time
import os
from minitask.worker.threadworker import Manager
from handofcats import as_command


def consume(m: Manager, uid: str) -> None:
    with m.open_reader_queue(uid) as q:
        for x in q:
            print(os.getpid(), "<-", x)
            time.sleep(0.1)


@as_command
def run() -> None:
    with Manager() as m:
        uid = m.generate_uid()
        m.spawn(consume, uid=uid)

        with m.open_writer_queue(uid) as q:
            for x in range(7):
                print(os.getpid(), "->", x)
                q.put(x)

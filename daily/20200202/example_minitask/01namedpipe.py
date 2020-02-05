from minitask.worker.namedpipeworker import Manager
from handofcats import as_command


def worker(m: Manager, uid):
    import os

    with m.open_reader_queue(uid) as q:
        for item in q:
            print(os.getpid(), item)


@as_command
def run():
    with Manager() as m:
        uid = m.generate_uid()
        m.spawn(worker, uid=uid)
        with m.open_writer_queue(uid) as q:
            for i in range(5):
                q.put(i)
            # q.put(None)

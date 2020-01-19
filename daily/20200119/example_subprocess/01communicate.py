import multiprocessing
import os
from handofcats import as_subcommand


@as_subcommand
def master() -> None:
    q = multiprocessing.Queue()
    port0, port1 = multiprocessing.Pipe()

    p = multiprocessing.Process(
        target=worker, kwargs={"q": q, "port": port1}, daemon=True
    )

    p.start()
    for i in range(10):
        q.put(i)
    q.put(None)
    pid = os.getpid()
    for x in iter(port0.recv, None):
        print(pid, x)
    p.join()


def worker(*, q: multiprocessing.Queue, port: multiprocessing.Pipe):
    # TODO: use JoinableQueue
    while not q.empty():
        pid = os.getpid()
        x = q.get()
        if x is None:
            break
        print(pid, "<<", x)
        import time

        time.sleep(0.5)
        print(pid, ">>", x)
        port.send(f"<<{x}>>")

    # todo: finaize
    port.send(None)
    port.close()


as_subcommand.run()

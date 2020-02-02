import typing as t
import queue
import threading
import time
import pathlib
from functools import cached_property
from minitask.q import Q
from minitask.q import PickleFormat
from minitask.q import consume
from minitask.communication import namedpipe
from handofcats import as_subcommand


def open_port(endpoint: str, mode):
    if mode == "r":
        return namedpipe.create_reader_port(endpoint)
    path = pathlib.Path(endpoint)
    if path.exists():
        path.unlink(missing_ok=True)
    return namedpipe.create_writer_port(str(path))


class QueueLike:
    def __init__(self, port) -> None:
        self.port = port

    def put(self, b: bytes):
        namedpipe.write(b, file=self.port)

    def get(self) -> bytes:
        b = namedpipe.read(file=self.port)
        if not b:
            return None
        return b

    def task_done(self):
        pass  # hmm


@as_subcommand
def run():
    import sys
    import subprocess

    endpoint = "x.fifo"
    subprocess.Popen([sys.executable, __file__, "worker", "--endpoint", endpoint])

    with open_port(endpoint, "w") as wf:
        q = Q(QueueLike(wf), format_protcol=PickleFormat())
        for i in range(5):
            q.put(i)
        time.sleep(0.05)
        q.put(None)
    print("ok")


@as_subcommand
def worker(*, endpoint: str):
    with open_port(endpoint, "r") as rf:
        q = Q(QueueLike(rf), format_protcol=PickleFormat())
        for item in consume(q):
            print(item)
    print("end")


as_subcommand.run()

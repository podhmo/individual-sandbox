import typing as t
import sys
import time
import pathlib
from minitask.q import Q, QueueLike, PickleFormat
from minitask.communication import namedpipe
from handofcats import as_subcommand


def fullname(ob: t.Any) -> str:
    return f"{sys.modules[ob.__module__].__file__}:{ob.__name__}"


class Executor:
    def __init__(self):
        self.processes = []

    def spawn(self, target, *, endpoint, format_protocol=None, communication=None):
        import sys
        import subprocess

        if format_protocol is not None:
            format_protocol = fullname(format_protocol)
        else:
            format_protocol = "minitask.q:PickleFormat"

        if communication is not None:
            communication = fullname(communication)
        else:
            communication = "minitask.communication.namedpipe"

        cmd = [
            sys.executable,
            "-m",
            "minitask.tool",
            "worker",
            "--endpoint",
            endpoint,
            "--format-protocol",
            format_protocol,
            "--handler",
            fullname(target),
            "--communication",
            communication,
        ]

        p = subprocess.Popen(cmd)
        self.processes.append(p)
        return p

    def wait(self):
        for p in self.processes:
            p.wait()


def open_port(endpoint: str, mode):
    if mode == "r":
        return namedpipe.create_reader_port(endpoint)
    path = pathlib.Path(endpoint)
    if path.exists():
        path.unlink(missing_ok=True)
    return namedpipe.create_writer_port(str(path))


@as_subcommand
def run():
    endpoint = str((pathlib.Path(__file__).parent / "x.fifo").absolute())
    ex = Executor()
    ex.spawn(worker, endpoint=endpoint)

    with open_port(endpoint, "w") as wf:
        q = Q(QueueLike(wf), format_protocol=PickleFormat())
        for i in range(5):
            q.put(i)
        time.sleep(0.05)
        q.put(None)
    ex.wait()
    print("ok")


def worker(q: Q):
    from minitask.q import consume

    for item in consume(q):
        print("<-", item)


as_subcommand.run()

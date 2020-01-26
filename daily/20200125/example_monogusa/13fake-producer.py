import contextlib
from collections import defaultdict
from functools import partial
import queue

"""
def producer(*, endpoint: str):
    import os

    ipc = minitask.IPC()
    pid = os.getpid()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            # msgpackrpc not support kwargs
            x.send("say", args=["hello", [pid]])
            time.sleep(0.1)
"""


class FakeIPC:
    def __init__(self, mapping):
        self.mapping = mapping

    @contextlib.contextmanager
    def serve(cls, endpoint: str):
        sender = FakeSender(cls, endpoint)
        yield sender
        sender.q.put_nowait(None)

    @contextlib.contextmanager
    def connect(cls, endpoint: str):
        yield FakeReciver(cls, endpoint)


class FakeSender:
    def __init__(self, ipc, endpoint: str):
        self.ipc = ipc
        self.endpoint = endpoint
        self.q = self.ipc.mapping[endpoint]

    def send(self, name, *, args=None, kwargs=None):
        return self.q.put_nowait({"name": name, "args": args, "kwargs": kwargs})


class FakeReciver:
    def __init__(self, ipc, endpoint: str):
        self.ipc = ipc
        self.endpoint = endpoint
        self.q = self.ipc.mapping[endpoint]

    def recv(self):
        return self.q.get()


class minitask:
    mapping = defaultdict(queue.Queue)

    IPC = partial(FakeIPC, mapping)


def producer(*, endpoint: str):
    import os

    pid = os.getpid()
    ipc = minitask.IPC()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", args=(["hello", [pid]]))


producer(endpoint="xxx")

ipc = minitask.IPC()
with ipc.connect("xxx") as x:
    while True:
        msg = x.recv()
        if msg is None:
            break
        print(msg)

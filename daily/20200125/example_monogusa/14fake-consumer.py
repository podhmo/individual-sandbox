import contextlib
from collections import defaultdict
from functools import partial
import queue

# template
"""
@executor.register
def consumer(*, endpoint: str):
    ipc = minitask.IPC()
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)
"""


class FakeIPC:
    def __init__(self, mapping):
        from tinyrpc.protocols import jsonrpc

        self.mapping = mapping
        self.serialization = jsonrpc.JSONRPCProtocol()

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
        req = self.ipc.serialization.create_request(
            name, args=args or [], kwargs=kwargs or {}
        )
        return self.q.put_nowait(req.serialize())


class FakeReciver:
    def __init__(self, ipc, endpoint: str):
        self.ipc = ipc
        self.endpoint = endpoint
        self.q = self.ipc.mapping[endpoint]

    def recv(self):
        body = self.q.get()
        if body is None:
            return body
        return self.ipc.serialization.parse_request(body)


class minitask:
    mapping = defaultdict(queue.Queue)

    IPC = partial(FakeIPC, mapping)


def consumer(*, endpoint: str):
    ipc = minitask.IPC()
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)


def producer(*, endpoint: str):
    import os

    pid = os.getpid()
    ipc = minitask.IPC()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", args=(["hello", [pid]]))


producer(endpoint="xxx")
consumer(endpoint="xxx")

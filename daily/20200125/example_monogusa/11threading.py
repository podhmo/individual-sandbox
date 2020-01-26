from __future__ import annotations
import time
import tempfile
import queue
import threading
import logging
from collections import defaultdict
from functools import partial
from handofcats import as_subcommand
import minitask2 as minitask

logger = logging.getLogger(__name__)


class Executor:
    def __init__(self):
        self.actions = {}
        self._tempdir = None
        self.dirpath = None

    def register(self, fn):
        self.actions[fn] = fn.__name__
        return fn

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        pass

    def spawn(self, fn, **kwargs):
        assert fn in self.actions
        action = partial(fn, **kwargs)
        th = threading.Thread(target=action, daemon=True)  # daemon = True?
        th.start()
        return _ProcessAdapter(th)

    def create_endpoint(self, *, uid: int):
        return uid


class _IOAdapter:
    def __init__(self, q: queue.Queue):
        self.q = q

    def close(self):
        self.q.put_nowait(None)


class _ProcessAdapter:
    def __init__(self, th: threading.Thread) -> None:
        self.th = th

    def wait(self):
        self.th.join()

    def terminate(self):
        # send to None?
        pass  # daemon=True, so ignored


class network:
    """ network layer"""

    mapping = {}
    sems = defaultdict(threading.Event)

    @classmethod
    def create_writer_port(cls, endpoint: str) -> _IOAdapter:
        port = _IOAdapter(queue.Queue())
        cls.mapping[endpoint] = port
        cls.sems[endpoint].set()
        return port

    @classmethod
    def create_reader_port(cls, endpoint: str) -> _IOAdapter:
        cls.sems[endpoint].wait()
        return cls.mapping[endpoint]

    @staticmethod
    def send(body: bytes, *, port: _IOAdapter):
        size = len(body)
        logger.debug("send	size:%d	body:%r", size, body)
        port.q.put(body)

    @staticmethod
    def recv(*, port: _IOAdapter) -> bytes:
        body = port.q.get()
        if body is None:
            return ""
        size = len(body)
        logger.debug("recv	size:%s	body:%r", size, body)
        return body


executor = Executor()


@executor.register
@as_subcommand
def run(*, keep: bool = False):
    with executor:
        pairs = []
        n = 2

        for uid in range(n):
            endpoint = executor.create_endpoint(uid=uid)
            sp = executor.spawn(producer, endpoint=endpoint)
            cp = executor.spawn(consumer, endpoint=endpoint)
            pairs.append((sp, cp))

        # todo: fix
        for sp, cp in pairs:
            sp.wait()
            cp.terminate()


@executor.register
@as_subcommand
def producer(*, endpoint: str):
    import os

    ipc = minitask.IPC(network=network)
    pid = os.getpid()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            # msgpackrpc not support kwargs
            x.send("say", args=["hello", [pid]])
            time.sleep(0.1)


@executor.register
@as_subcommand
def consumer(*, endpoint: str):
    ipc = minitask.IPC(network=network)
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)


as_subcommand.run()

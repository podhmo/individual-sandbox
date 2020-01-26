import threading
from collections import defaultdict
import logging
import queue
import minitask2 as minitask

logger = logging.getLogger(__name__)

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


class _IOAdapter:
    def __init__(self, q: queue.Queue):
        self.q = q

    def close(self):
        self.q.put_nowait(None)  # auto finalize?


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


def consumer(*, endpoint: str):
    ipc = minitask.IPC(network=network)
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)


def producer(*, endpoint: str):
    import os

    pid = os.getpid()
    ipc = minitask.IPC(network=network)
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", args=(["hello", [pid]]))


producer(endpoint="xxx")
consumer(endpoint="xxx")

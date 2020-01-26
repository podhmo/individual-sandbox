from __future__ import annotations
import typing as t
from collections import defaultdict
from functools import partial
import itertools
import os
import sys
import pathlib
import tempfile
import time
import subprocess
import threading
import queue
import logging

from tinyrpc.protocols import RPCRequest  # , RPCResponse, RPCErrorResponse
from handofcats import as_subcommand

logger = logging.getLogger(__name__)


class network:
    """ network layer"""

    @staticmethod
    def create_writer_port(endpoint: str) -> t.IO[bytes]:
        def _opener(path: str, flags: int) -> int:
            return os.open(path, os.O_WRONLY)  # NOT O_CREAT

        logger.info("create fifo: %s", endpoint)
        os.mkfifo(str(endpoint))  # TODO: force option?
        return open(endpoint, "wb", opener=_opener)

    @staticmethod
    def create_reader_port(
        endpoint: str, retries=[0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4, 12.8]
    ) -> t.IO[bytes]:
        for i, waittime in enumerate(retries, 1):
            try:
                io = open(endpoint, "rb")
                return io
            except FileNotFoundError:
                time.sleep(waittime)
                logger.debug("%r is not found, waiting, retry=%d", endpoint, i)

    @staticmethod
    def send(body: bytes, *, port: t.IO[bytes]):
        size = len(body)

        port.write(str(size).encode("utf-8"))  # todo: encoding
        port.write(b"\n")
        port.write(body)
        logger.debug("send	size:%d	body:%r", size, body)
        port.flush()

    @staticmethod
    def recv(*, port: t.IO[bytes]) -> bytes:
        size = port.readline()
        if not size:
            return ""
        body = port.read(int(size))
        logger.debug("recv	size:%s	body:%r", size, body)
        return body


class fake_network:
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


class _IOAdapter:
    def __init__(self, q: queue.Queue):
        self.q = q

    def close(self):
        self.q.put_nowait(None)  # auto finalize?


class _ProcessAdapter:
    def __init__(self, th: threading.Thread) -> None:
        self.th = th

    def wait(self):
        self.th.join()

    def terminate(self):
        # send to None?
        pass  # daemon=True, so ignored


class ThreadingEnvironment:
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


class SubprocessEnvironment:
    def __init__(self, *, as_subcommand=as_subcommand):
        self.actions = {}
        self._tempdir = None
        self.dirpath = None

        self._as_subcommand = as_subcommand
        self.run = self._as_subcommand.run

    def register(self, fn, name=None):
        self.actions[fn] = name or fn.__name__
        return self._as_subcommand(fn)

    def __enter__(self):
        self._tempdir = tempfile.TemporaryDirectory()
        self.dirpath = self._tempdir.__enter__()
        return self

    def __exit__(self, typ, val, tb):
        if self._tempdir is not None:
            return self._tempdir.__exit__(typ, val, tb)

    def spawn(self, fn, **kwargs):
        name = self.actions[fn]
        args = itertools.chain.from_iterable([(f"--{k}", v) for k, v in kwargs.items()])
        return subprocess.Popen([sys.executable, __file__, name, *args])

    def create_endpoint(self, *, uid: int):
        return pathlib.Path(self.dirpath) / f"worker.{uid}.fifo"


class IPC:
    # from tinyrpc.protocols import msgpackrpc

    # def __init__(
    #     self, *, serialization=msgpackrpc.MSGPACKRPCProtocol(), network=network
    # ):
    from tinyrpc.protocols import jsonrpc

    def __init__(self, *, serialization=jsonrpc.JSONRPCProtocol(), network=network):
        self.serialization = serialization
        self.network = network

    def connect(self, endpoint: str,) -> InternalRecv:
        io = self.network.create_reader_port(endpoint)
        return InternalRecv(io, serialization=self.serialization, network=self.network)

    def serve(self, endpoint: str,) -> InternalRecv:
        io = self.network.create_writer_port(endpoint)
        return InternalSend(io, serialization=self.serialization, network=self.network)


class InternalRecv:
    def __init__(self, io: t.IO[bytes], *, serialization, network) -> None:
        self.io = io
        self.serialization = serialization
        self.network = network

    def recv(self) -> RPCRequest:
        msg = self.network.recv(port=self.io)
        if not msg:
            return None
        return self.serialization.parse_request(msg)

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        # TODO: exception is raised.

        if self.io is not None:
            self.io.close()
            self.io = None  # TODO: lock? (semaphore?)


class InternalSend:
    def __init__(self, io: t.IO[bytes], *, serialization, network) -> None:
        self.io = io
        self.serialization = serialization
        self.network = network

    def send(
        self,
        method: str,
        args: t.List[t.Any] = None,
        kwargs: t.Dict[str, t.Any] = None,
        one_way: bool = False,
    ):
        req = self.serialization.create_request(
            method, args=args, kwargs=kwargs, one_way=one_way
        )
        return self.network.send(req.serialize(), port=self.io)

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        if self.io is not None:
            self.io.close()
            self.io = None  # TODO: lock?

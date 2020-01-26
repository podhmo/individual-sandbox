from __future__ import annotations
import typing as t
import logging
import time
import os
from tinyrpc.protocols import RPCRequest  # , RPCResponse, RPCErrorResponse

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
        if self.io is not None:
            self.io.close()
            self.io = None  # TODO: lock?


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

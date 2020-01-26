from __future__ import annotations
import typing as t
import logging
import os
from tinyrpc.protocols import jsonrpc
from tinyrpc.protocols import RPCRequest, RPCResponse, RPCErrorResponse

logger = logging.getLogger(__name__)
protocol = jsonrpc.JSONRPCProtocol()


class communication:
    """ communication layer"""

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


class serialization:
    p = jsonrpc.JSONRPCProtocol()

    @classmethod
    def create_request(
        cls,
        method: str,
        args: t.List[t.Any] = None,
        kwargs: t.Dict[str, t.Any] = None,
        one_way: bool = False,
    ) -> RPCRequest:
        return cls.p.create_request(
            method, args=args or [], kwargs=kwargs or {}, one_way=one_way
        )

    @classmethod
    def parse_request(cls, body: bytes) -> RPCRequest:
        return cls.p.parse_request(body)


class IPC:
    def __init__(self, *, serialization=serialization, communication=communication):
        self.serialization = serialization
        self.communication = communication

    def create_endpoint(self, endpoint: str):  # TODO: rename?
        logger.info("create fifo: %s", endpoint)
        os.mkfifo(str(endpoint))  # TODO: force option?
        return endpoint

    def connect(self, endpoint: str,) -> InternalRecv:
        # XXX: always need create_endpoint(), before using this method
        io = open(endpoint, "rb")  # TODO:
        return InternalRecv(
            io, serialization=self.serialization, communication=self.communication
        )

    def serve(self, endpoint: str,) -> InternalRecv:
        # XXX: always need create_endpoint(), before using this method
        def _opener(path: str, flags: int) -> int:
            return os.open(path, os.O_WRONLY)  # NOT O_CREAT

        io = open(endpoint, "wb", opener=_opener)
        return InternalSend(
            io, serialization=self.serialization, communication=self.communication
        )


class InternalRecv:
    def __init__(self, io: t.IO[bytes], *, serialization, communication) -> None:
        self.io = io
        self.serialization = serialization
        self.communication = communication

    def recv(self) -> RPCRequest:
        msg = self.communication.recv(port=self.io)
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
    def __init__(self, io: t.IO[bytes], *, serialization, communication) -> None:
        self.io = io
        self.serialization = serialization
        self.communication = communication

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
        return self.communication.send(req.serialize(), port=self.io)

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        if self.io is not None:
            self.io.close()
            self.io = None  # TODO: lock?

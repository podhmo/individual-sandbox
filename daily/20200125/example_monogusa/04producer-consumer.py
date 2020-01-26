import typing as t
import logging
import json
from handofcats import as_subcommand
import sys
import subprocess

logger = logging.getLogger(__name__)

"""
layerを分ける

- communication layer -- namedpipe
- marshaller/unmarshaller -- json (TODO: using jsonrpc protocol?)
- handling object
"""


class InternalConsumer:
    def __init__(self, fifo_path: str) -> None:
        self.fifo_path = fifo_path
        self.io = None

    def __enter__(self) -> "InternalConsumer":
        self.io = open(self.fifo_path)
        return self

    def __exit__(self, type_, val, tb):
        if self.io is not None:
            self.io.close()

    # TODO: unmarshaller
    # TODO: timeout
    # TODO: internal q
    # TODO: asyncio support
    def recv(self):
        msg = recv(port=self.io)
        if not msg:
            return None
        return json.loads(msg)

    def __iter__(self):
        return iter(self.recv, None)


class InternalProducer:
    def __init__(self, fifo_path: str) -> None:
        self.fifo_path = fifo_path
        self.io = None

    @classmethod
    def _opener(cls, path: str, flags: int) -> int:
        import os

        return os.open(path, os.O_WRONLY)  # NOT O_CREAT

    def __enter__(self) -> "InternalConsumer":
        self.io = open(self.fifo_path, "w", opener=self._opener)
        return self

    def __exit__(self, type_, val, tb):
        if self.io is not None:
            self.io.close()

    def send(self, msg) -> None:
        body = json.dumps(msg)
        return send(body, port=self.io)


def send(body: str, *, port: t.IO[str]):
    size = len(body)

    print(size, file=port)
    print(body, file=port, end="")

    logger.debug("send	size:%d	body:%r", size, body)
    port.flush()


def recv(*, port: t.IO[str]) -> str:
    size = port.readline()
    if not size:
        return ""
    body = port.read(int(size))
    logger.debug("recv	size:%s	body:%r", size, body)
    return body


@as_subcommand
def run(*, keep: bool = False):
    import os
    import pathlib
    import tempfile

    # TODO: keep option

    with tempfile.TemporaryDirectory() as dirpath:
        dirpath = pathlib.Path(dirpath)

        pairs = []
        n = 1
        for i in range(n):
            endpoint = path = dirpath / f"worker.{i}.fifo"
            logger.info("create fifo: %s", path)
            os.mkfifo(str(path))

            sp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "producer",
                    "--endpoint",
                    endpoint,
                    "--uid",
                    str(i),
                ]
            )
            cp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "consumer",
                    "--endpoint",
                    endpoint,
                    "--uid",
                    str(i),
                ]
            )
            pairs.append((sp, cp))

        for sp, cp in pairs:
            sp.wait()
            cp.terminate()


@as_subcommand
def producer(*, endpoint: str, uid: int):
    import time

    with InternalProducer(endpoint) as p:
        for i in range(5):
            p.send({"method": "hello", "kwargs": {"uid": uid}})
            time.sleep(0.1)


@as_subcommand
def consumer(*, endpoint: str, uid: int):
    with InternalConsumer(endpoint) as c:
        for msg in c:
            print(uid, "got", msg)


as_subcommand.run()

import typing as t
import logging
import json
from functools import partial
from handofcats import as_subcommand
import sys
import subprocess

logger = logging.getLogger(__name__)

"""
send/recvできるようにする
"""


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
    import os

    # NOT O_CREAT
    def _opener(path: str, flags: int) -> int:
        return os.open(path, os.O_WRONLY)

    with open(endpoint, "w", opener=_opener) as wf:
        for i in range(5):
            send(json.dumps({"method": "hello", "kwargs": {"uid": uid}}), port=wf)
            time.sleep(0.1)


@as_subcommand
def consumer(*, endpoint: str, uid: int):
    with open(endpoint) as rf:
        for msg in iter(partial(recv, port=rf), ""):
            print(uid, "got", json.loads(msg))


as_subcommand.run()

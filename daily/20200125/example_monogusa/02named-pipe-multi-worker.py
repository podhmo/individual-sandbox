import typing as t
import logging
from handofcats import as_subcommand
import sys
import subprocess

logger = logging.getLogger(__name__)


@as_subcommand
def run(*, keep: bool = False):
    import os
    import pathlib
    import tempfile

    # TODO: keep option

    with tempfile.TemporaryDirectory() as dirpath:
        dirpath = pathlib.Path(dirpath)

        pairs = []
        for i in range(5):
            endpoint = path = dirpath / f"worker.{i}.fifo"
            logger.info("create fifo: %s", path)
            os.mkfifo(str(path))

            sp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "server",
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
                    "client",
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
def server(*, endpoint: str, uid: int):
    import time
    import os

    # NOT O_CREAT
    def _opener(path: str, flags: int) -> int:
        return os.open(path, os.O_WRONLY)

    with open(endpoint, "w", opener=_opener) as wf:
        for i in range(5):
            msg = f"hello {i}"
            logger.info("send: %r", msg)
            print(uid, msg, file=wf)
            wf.flush()
            time.sleep(0.1)


@as_subcommand
def client(*, endpoint: str, uid: int):
    with open(endpoint) as rf:
        for msg in rf:
            logger.info("recv: %r", msg)
            print(uid, "got", msg)


as_subcommand.run()

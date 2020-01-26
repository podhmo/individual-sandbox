import typing as t
import logging
from handofcats import as_subcommand
import sys
import subprocess

logger = logging.getLogger(__name__)


@as_subcommand
def run(*, endpoint: str = "/tmp/worker.fifo.0"):
    import os
    import pathlib

    path = pathlib.Path(endpoint)
    if path.exists():
        logger.info("remove fifo: %s", path)
        path.unlink(missing_ok=True)

    try:
        logger.info("create fifo: %s", path)
        os.mkfifo(str(path))

        sp = subprocess.Popen(
            [sys.executable, __file__, "server", "--endpoint", endpoint]
        )
        cp = subprocess.Popen(
            [sys.executable, __file__, "client", "--endpoint", endpoint]
        )
        sp.wait()
        cp.terminate()
    finally:
        if path.exists():
            logger.info("remove fifo: %s", path)
            path.unlink(missing_ok=True)
    print("ok")


@as_subcommand
def server(*, endpoint: str):
    import time
    import os

    # NOT O_CREAT
    def _opener(path: str, flags: int) -> int:
        return os.open(path, os.O_WRONLY)

    with open(endpoint, "w", opener=_opener) as wf:
        for i in range(5):
            msg = f"hello {i}"
            logger.info("send: %r", msg)
            print(msg, file=wf)
            wf.flush()
            time.sleep(0.1)


@as_subcommand
def client(*, endpoint: str):
    with open(endpoint) as rf:
        for msg in rf:
            logger.info("recv: %r", msg)
            print("got", msg)


as_subcommand.run()

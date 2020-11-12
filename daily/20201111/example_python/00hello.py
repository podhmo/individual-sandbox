from __future__ import annotations
import typing as t
import sys
import tempfile
import pathlib
import time
import subprocess
import logging
import contextlib
import os
import httpx
from handofcats import as_command


logger = logging.getLogger(__name__)


def _find_free_port(host: str = "0.0.0.0") -> int:
    import socket

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind((host, 0))
        sock.listen(1)
        name, port = sock.getsockname()  # type: t.Tuple[str, int]
    return port


@contextlib.contextmanager
def connect_server(
    server_process: subprocess.Popen, *, sentinel: str,
) -> t.Iterator[t.subprocess.Popen]:
    try:
        p = pathlib.Path(sentinel)
        with p.open("w"):
            pass

        ok = False
        st = time.time()
        for connect_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if p.exists():
                print(f"	connect ... {connect_time}", file=sys.stderr)
                time.sleep(connect_time)
                continue
            ok = True
            break
        if not ok:
            raise RuntimeError(f"timeout, f{sentinel}, {time.time()-st}")
        yield server_process
    finally:
        try:
            server_process.terminate()
            server_process.wait()
        except Exception as e:
            logger.info(str(e), exc_info=True)


@as_command
def run(*, port: int):
    port = _find_free_port()
    sentinel = tempfile.mktemp()

    cmd = [
        sys.executable,
        "-m",
        "uvicorn",
        "x00hello:app",
        "--port",
        str(port),
        "--no-use-colors",
    ]
    server_process = subprocess.Popen(cmd, env={"SENTINEL": sentinel})

    # TODO:
    # - headers
    # - cookies
    # - queries
    # - trace (logging)
    # - retry
    # - auth
    # - post JSON
    # - post formData
    with connect_server(server_process, sentinel=sentinel):
        print("----------------------------------------")
        url = f"http://localhost:{port}/"
        res = httpx.request("GET", url, params={"pretty": True})
        print(res, res.text)
        print(res.json())
        print("----------------------------------------")

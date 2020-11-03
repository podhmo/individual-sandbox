from __future__ import annotations
import typing as t
import sys
import tempfile
import pathlib
import time
import subprocess
import logging
import contextlib
import zmq
from tinyrpc import RPCClient
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.transports.zmq import ZmqClientTransport
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
    cmd: t.List[str], *, sentinel: str, port: int
) -> t.Iterator[t.subprocess.Popen]:
    assert sentinel in cmd
    assert str(port) in cmd

    server_process = subprocess.Popen(cmd)
    try:
        p = pathlib.Path(sentinel)
        with p.open("w"):
            pass

        ok = False
        st = time.time()
        for wait_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if p.exists():
                print(f"	wait ... {wait_time}", file=sys.stderr)
                time.sleep(wait_time)
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
def run() -> None:
    ctx = zmq.Context()

    port = _find_free_port()
    sentinel = tempfile.mktemp()

    cmd = [sys.executable, "03server.py", "--port", str(port), "--sentinel", sentinel]
    with connect_server(cmd, sentinel=sentinel, port=port):
        print(f"connect ... {port}", file=sys.stderr)
        rpc_client = RPCClient(
            JSONRPCProtocol(), ZmqClientTransport.create(ctx, f"tcp://127.0.0.1:{port}")
        )
        s = rpc_client.get_proxy()

        # call a method called 'reverse_string' with a single string argument
        print(s.pow(2, 3))  # Returns 2**3 = 8
        print(s.add(2, 3))  # Returns 5
        print(s.mul(5, 2))  # Returns 5*2 = 10

        # Print list of available methods
        print(s.list_methods())

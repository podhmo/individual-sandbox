import sys
import tempfile
import pathlib
import time
import subprocess
import logging
import xmlrpc.client
import contextlib
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
def connect_server(*, sentinel: str, port: int):
    cmd = [sys.executable, "01server.py", "--port", str(port), "--sentinel", sentinel]
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
    sentinel = tempfile.mktemp()
    port = _find_free_port()

    with connect_server(sentinel=sentinel, port=port):
        print(f"connect ... {port}", file=sys.stderr)
        s = xmlrpc.client.ServerProxy(f"http://localhost:{port}")
        print(s.pow(2, 3))  # Returns 2**3 = 8
        print(s.add(2, 3))  # Returns 5
        print(s.mul(5, 2))  # Returns 5*2 = 10

        # Print list of available methods
        print(s.system.listMethods())

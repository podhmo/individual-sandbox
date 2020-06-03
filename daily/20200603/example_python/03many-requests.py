import typing as t
import tempfile
import subprocess
import sys
import pathlib
from handofcats import as_subcommand
from findport import free_port
import logging

logger = logging.getLogger(__name__)


@as_subcommand
def serve(*, port: int, host: str = "", sentinel: t.Optional[str] = None) -> None:
    from wsgiref.simple_server import make_server

    def app(environ, start_response):
        status = "200 OK"
        headers = [("Content-type", "application/json; charset=utf-8")]
        start_response(status, headers)
        return [b'{"message": "ok"}']

    with make_server(host, port, app) as server:
        print("init")
        import signal

        def shutdown(signum, tb):
            print("!!", signum)
            server.shutdown()
            print("server shutdown")

        signal.signal(signal.SIGTERM, shutdown)
        signal.signal(signal.SIGINT, shutdown)
        import threading

        th = threading.Thread(target=server.serve_forever)
        th.start()
        if sentinel is not None:
            if pathlib.Path(sentinel).exists():
                # 3.8ならunlink(missing_ok=True)が使えるのに
                pathlib.Path(sentinel).unlink()
                logger.info("remove sentinel %s", sentinel)

        th.join()
    print("process end")


@as_subcommand
def run() -> None:
    port = free_port()
    fd, sentinel = tempfile.mkstemp()
    logger.info("create sentinel %s", sentinel)
    p = subprocess.Popen(
        [sys.executable, __file__, "serve", "--port", str(port), "--sentinel", sentinel]
    )
    import time
    import requests

    while True:
        if not pathlib.Path(sentinel).exists():
            logger.debug("connected")
            break
        logger.debug("wait")
        time.sleep(0.1)  # todo: backoff

    for i in range(5):
        res = requests.get(f"http://localhost:{port}")
        res.raise_for_status()
        print(res.json())
    p.terminate()  # send SIGTERM
    p.wait()


as_subcommand.run()

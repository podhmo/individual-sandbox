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
    import time
    import requests

    port = free_port()

    def setup() -> subprocess.Popen:
        fd, sentinel = tempfile.mkstemp()
        logger.info("create sentinel %s", sentinel)

        p = subprocess.Popen(
            [
                sys.executable,
                __file__,
                "serve",
                "--port",
                str(port),
                "--sentinel",
                sentinel,
            ]
        )
        retries = [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4, 12.8]
        start_time = time.time()
        passed_time = None

        for wait_time in retries:
            if not pathlib.Path(sentinel).exists():
                logger.debug("connected")
                passed_time = time.time()
                break
            logger.debug("wait")
            time.sleep(wait_time)  # todo: backoff

        if passed_time is None:
            exc = TimeoutError(f"{time.time() - start_time} sec passed, p={p.args}")
            logger.warning("hmm %r, kill process", exc)
            p.kill()
            raise exc
        return p

    with setup() as p:
        for i in range(5):
            res = requests.get(f"http://localhost:{port}")
            res.raise_for_status()
            print(res.json())
        p.terminate()  # send SIGTERM
        p.wait()


as_subcommand.run()

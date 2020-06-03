import typing as t
import pathlib
from handofcats import as_command
import logging

logger = logging.getLogger(__name__)


@as_command
def serve(*, port: int, host: str = "", sentinel: t.Optional[str] = None) -> None:
    from wsgiref.simple_server import make_server

    def app(environ, start_response):
        status = "200 OK"
        headers = [("Content-type", "application/json; charset=utf-8")]
        start_response(status, headers)
        return [b'{"message": "ok"}']

    with make_server(host, port, app) as server:
        import signal

        def shutdown(signum, tb):
            logger.info("signal received: %s", signal.getsignal(signum))
            server.shutdown()
            print("server shutdown")

        signal.signal(signal.SIGTERM, shutdown)
        signal.signal(signal.SIGINT, shutdown)
        import threading

        th = threading.Thread(target=server.serve_forever)
        th.start()
        if sentinel is not None:
            if pathlib.Path(sentinel).exists():
                pathlib.Path(sentinel).unlink()
                logger.info("remove sentinel %s", sentinel)

        th.join()
    print("process end")

from egoist.app import App
import logging

NAME = __name__
logger = logging.getLogger(__name__)


def includeme(app: App):
    app.include("discovery")

    # TODO: only spawn, when using
    def _register():
        import sys
        import pathlib
        from egoist.internal.netutil import find_free_port
        from discovery import get_discovery
        import util

        sentinel = util.create_sentinel_file()
        server_py = pathlib.Path(__file__).absolute().with_name("server.py")
        port = find_free_port()

        get_discovery().register("HELLO", url=f"http://127.0.0.1:{port}")

        argv = [
            sys.executable,
            server_py,
            "--port",
            str(port),
            "--host",
            "127.0.0.1",
            "--sentinel",
            sentinel,
        ]

        p = util.ConnectedProcess().spawn(argv, sentinel=sentinel)
        import atexit

        def _shutdown():
            logger.info("terminate HELLO")
            with p:
                p.terminate()

        atexit.register(_shutdown)

    app.action(NAME, _register)

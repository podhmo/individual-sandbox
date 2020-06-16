from egoist.app import App
import logging

NAME = __name__
logger = logging.getLogger(__name__)


def includeme(app: App):
    app.include("discovery")

    # TODO: only spawn, when using
    def _register():
        import shutil
        from egoist.internal.netutil import find_free_port
        from discovery import get_discovery
        import util

        sentinel = util.create_sentinel_file()
        port = find_free_port()

        get_discovery().register("gofmtrpc", url=f"http://127.0.0.1:{port}")

        assert shutil.which("gofmtrpc")
        argv = [
            "gofmtrpc",
            "-addr",
            f":{port}",
            "-sentinel",
            sentinel,
        ]
        p = util.ConnectedProcess().spawn(argv, sentinel=sentinel)
        import atexit

        def _shutdown():
            logger.info("terminate gofmtrpc")
            with p:
                p.terminate()

        atexit.register(_shutdown)

    app.action(NAME, _register)

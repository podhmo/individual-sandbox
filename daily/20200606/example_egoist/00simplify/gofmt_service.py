from egoist.app import App
import logging

NAME = __name__
logger = logging.getLogger(__name__)


def includeme(app: App):
    app.include("discovery")

    # TODO: spawn, when using
    # TODO: spawn, when dry_run=False only
    def _register():
        from discovery import get_discovery

        port = None

        if app.registry.dry_run:
            logger.info("dry run, %s skipped", NAME)
            get_discovery().register("gofmtrpc", url=f"http://127.0.0.1:{port}")
            return

        from egoist.internal.netutil import find_free_port
        from util import spawn_with_connection

        port = find_free_port()
        get_discovery().register("gofmtrpc", url=f"http://127.0.0.1:{port}")

        argv = ["gofmtrpc", "-addr", f":{port}"]
        p, _ = spawn_with_connection(argv, sentinel_option="-sentinel")

        import atexit

        def _shutdown():
            logger.info("terminate gofmtrpc")
            with p:
                p.terminate()

        atexit.register(_shutdown)

    app.action(NAME, _register)

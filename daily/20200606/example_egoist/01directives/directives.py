import typing as t
import logging
from egoist.app import App

NAME = __name__
logger = logging.getLogger(__name__)


def define_server_process(app: App):
    def _define(
        app: App,
        fmt: str,
        *,
        name: str,
        urlfmt: str = "http://{host}:{port}",
        host: str = "127.0.0.1",
        port: t.Optional[int] = None,
        params: t.Optional[t.Dict[str, t.Callable[[], object]]] = None,
    ):
        def _register():
            nonlocal host
            nonlocal port

            import shlex
            import atexit
            from discovery import get_discovery
            from rpcutil import find_free_port

            kwargs = {k: fn(app) for k, fn in (params or {}).items()}
            if port is None:
                port = kwargs.get("port") or find_free_port()
            if "host" in kwargs:
                host = kwargs.get["host"]

            argv = shlex.split(fmt.format(**kwargs))
            url = urlfmt.format(host=host, port=port)

            get_discovery().register(name, url=url)
            if app.registry.dry_run:
                logger.info("dry run, %s skipped", name)
                return

            from spawn import spawn_with_connection

            p, _ = spawn_with_connection(argv)

            def _shutdown():  # xxx:
                logger.info("terminate %s", name)
                with p:
                    p.terminate()

            atexit.register(_shutdown)

        app.action(("define_server_process", name), _register)

    app.include("discovery")
    app.add_directive("define_server_process", _define)

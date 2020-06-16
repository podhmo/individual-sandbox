import typing as t
import typing_extensions as tx
import time
import pathlib
import subprocess
import logging


logger = logging.getLogger(__name__)


class ConnectionChecker(tx.Protocol):
    def ping(self) -> bool:
        """is connected?"""
        ...

    def pong(self) -> bool:
        """reply"""
        ...


class SentinelHandler(tx.Protocol):
    def create_sentinel(self) -> str:
        ...

    def inject_sentinel(self, argv: t.List[str], *, sentinel: str) -> t.List[str]:
        ...

    def create_connection_checker(self, *, sentinel: str) -> ConnectionChecker:
        ...


class FileSentinelHandler:  # SentinelHandler
    def __init__(self, option_name: str = "--sentinel") -> None:
        self.option_name = option_name

    def inject_sentinel(self, argv: t.List[str], *, sentinel: str) -> t.List[str]:
        if sentinel in argv:
            logger.debug("sentinel %s is included in %s", sentinel, argv)
            return argv
        return [*argv, self.option_name, sentinel]

    def create_sentinel(self) -> str:
        import tempfile

        fd, sentinel = tempfile.mkstemp()
        logger.info("create sentinel %s", sentinel)
        return sentinel

    def create_connection_checker(self, *, sentinel: str) -> ConnectionChecker:
        return FileConnectionChecker(sentinel=sentinel)


class FileConnectionChecker:  # ConnectionChecker
    def __init__(self, *, sentinel: str):
        self.sentinel = sentinel

    def ping(self) -> bool:
        return not pathlib.Path(self.sentinel).exists()

    def pong(self) -> bool:
        sentinel = self.sentinel
        if pathlib.Path(sentinel).exists():
            logger.info("remove sentinel %s", sentinel)
            pathlib.Path(sentinel).unlink()
            return True
        return False


def spawn_with_connection(
    argv: t.List[str],
    *,
    handler: t.Optional[SentinelHandler] = None,
    sentinel_option: str,
    retries: t.List[float] = [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4],
    check: bool = True,
) -> t.Tuple[subprocess.Popen, ConnectionChecker]:
    handler = handler or FileSentinelHandler(sentinel_option)
    sentinel = handler.create_sentinel()
    p = subprocess.Popen(handler.inject_sentinel(argv, sentinel=sentinel))
    checker = handler.create_connection_checker(sentinel=sentinel)

    if not check:
        return p, checker

    try:
        start_time = time.time()
        end_time = None

        for wait_time in retries:
            if checker.ping():
                end_time = time.time()
                logger.debug("connected")
                break

            logger.debug("wait: %f", wait_time)
            time.sleep(wait_time)  # todo: backoff

        if end_time is None:
            raise TimeoutError(f"{time.time() - start_time} sec passed, {p.args}")
        return p, checker
    except Exception as exc:
        logger.warning("hmm %r, kill process", exc)
        p.kill()  # kill?
        raise

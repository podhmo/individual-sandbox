import typing as t
import time
import pathlib
import subprocess
import logging
import contextlib


logger = logging.getLogger(__name__)


class ConnectedProcess:
    def ping(
        self, p: subprocess.Popen, *, sentinel: str, retries: t.List[float]
    ) -> float:
        start_time = time.time()

        for wait_time in retries:
            if not pathlib.Path(sentinel).exists():
                logger.debug("connected")
                return time.time() - start_time

            logger.debug("wait: %f", wait_time)
            time.sleep(wait_time)  # todo: backoff

        raise TimeoutError(f"{time.time() - start_time} sec passed, p={p.argv}")

    def pong(self, *, sentinel: str) -> bool:
        if pathlib.Path(sentinel).exists():
            logger.info("remove sentinel %s", sentinel)
            pathlib.Path(sentinel).unlink()
            return True
        return False

    @contextlib.contextmanager
    def spawn(
        self,
        argv: t.List[str],
        *,
        sentinel: str,
        inject_fn: t.Optional[t.Callable[[t.List[str], str], t.List[str]]] = None,
        retries: t.List[float] = [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4],
        check: bool = True,
    ) -> t.Iterator[subprocess.Popen]:
        inject_fn = inject_fn or _default_inject_sentinel
        argv = inject_fn(argv, sentinel)

        with subprocess.Popen(argv) as p:
            try:
                if check:
                    self.ping(p, sentinel=sentinel, retries=retries)
                yield p
                p.terminate()
            except Exception as exc:
                logger.warning("hmm %r, kill process", exc)
                p.kill()  # kill?
                raise


def _default_inject_sentinel(
    argv: t.List[str], sentinel: str, *, option_name: str = "--sentinel"
) -> t.List[str]:
    if sentinel in argv:
        logger.debug("sentinel %s is included in %s", sentinel, argv)
        return argv
    return [*argv, option_name, sentinel]


def create_sentinel_file() -> pathlib.Path:
    import tempfile

    fd, sentinel = tempfile.mkstemp()
    logger.info("create sentinel %s", sentinel)
    return sentinel

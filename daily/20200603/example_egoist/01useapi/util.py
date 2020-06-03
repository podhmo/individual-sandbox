import typing as t
import time
import pathlib

import subprocess
import logging

logger = logging.getLogger(__name__)


def create_sentinel_file() -> pathlib.Path:
    import tempfile

    fd, sentinel = tempfile.mkstemp()
    logger.info("create sentinel %s", sentinel)
    return sentinel


def create_service_process(
    *,
    argv: t.List[str],
    sentinel: str,
    retries: t.List[float] = [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4],
) -> subprocess.Popen:
    assert sentinel in argv

    p = subprocess.Popen(argv)
    start_time = time.time()
    passed_time = None

    for wait_time in retries:
        if not pathlib.Path(sentinel).exists():
            logger.debug("connected")
            passed_time = time.time()
            break
        logger.debug("wait: %f", wait_time)
        time.sleep(wait_time)  # todo: backoff

    if passed_time is None:
        exc = TimeoutError(f"{time.time() - start_time} sec passed, p={p.argv}")
        logger.warning("hmm %r, kill process", exc)
        p.kill()
        raise exc
    return p

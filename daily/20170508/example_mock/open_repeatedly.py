import time
import logging

logger = logging.getLogger(__name__)


def open_repeatedly(path, retries=5, retry_interval=1.0):
    while True:
        try:
            logger.info("open(%r)", path)
            return open(path)
        except OSError:
            if retries <= 0:
                raise
            if retry_interval > 0:
                time.sleep(retry_interval)
            retries -= 1

import logging
import time
import string

from concurrent.futures import ThreadPoolExecutor

logger = logging.getLogger(__name__)


def do_work(item):
    logger.info(f"start %s", item)
    time.sleep(0.2)
    logger.info(f"end   %s", item)


def source():
    return list(string.ascii_uppercase)


logging.basicConfig(level=logging.INFO, format="%(asctime)s " + logging.BASIC_FORMAT)
num_worker_threads = 10
with ThreadPoolExecutor(max_workers=num_worker_threads) as ex:
    for item in source():
        ex.submit(do_work, item)

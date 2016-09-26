import asyncio
import random
import logging
logger = logging.getLogger(__name__)

EMPTY = "empty"
FULL = "full"


def consume():
    n = random.random() > 0.5
    logger.info(n)
    return EMPTY if n else FULL


@asyncio.coroutine
def do_loop():
    while True:
        status = consume()
        logger.info("status: %s", status)
        if status == FULL:
            logger.info("sleep 3")
            yield from asyncio.sleep(3)
        else:
            yield from asyncio.sleep(1)

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())

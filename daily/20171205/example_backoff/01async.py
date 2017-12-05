import backoff
import logging
import asyncio
logger = logging.getLogger(__name__)


@backoff.on_exception(backoff.expo, (ZeroDivisionError, ), max_tries=5)
async def tick(i):
    logger.info("tick %s", i)
    return 1 / 0


async def run():
    x = tick(0)
    y = tick(1)
    await asyncio.gather(x, y)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO)
    loop = asyncio.get_event_loop()
    loop.run_until_complete(run())

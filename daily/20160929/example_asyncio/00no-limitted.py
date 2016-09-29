import asyncio
import logging
logger = logging.getLogger(__name__)


async def do_task(i):
    logger.info("start: %s", i)
    await asyncio.sleep(2)
    logger.info("end: %s", i)


async def do_loop():
    nums = range(10)
    futs = []
    for i in nums:
        futs.append(do_task(i))
    await asyncio.gather(*futs)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")

    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()

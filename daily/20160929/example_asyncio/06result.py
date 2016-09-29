import asyncio
import logging
logger = logging.getLogger(__name__)


async def do_task(i):
    logger.info("start: %s", i)
    await asyncio.sleep(0)
    logger.info("end: %s", i)
    return i

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    result = loop.run_until_complete(do_task(0))
    print(result)
    loop.close()

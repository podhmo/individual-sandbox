import asyncio
import logging
logger = logging.getLogger(__name__)

async def do_task():
    logger.info("start task")
    await asyncio.sleep(1)
    logger.info("end task")


async def do_loop():
    logger.info("start loop")
    # asyncio.ensure_future(do_task())
    # await do_task()
    await asyncio.sleep(2)
    logger.info("end loop")


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()

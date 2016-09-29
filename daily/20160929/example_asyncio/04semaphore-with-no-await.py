import asyncio
import logging
logger = logging.getLogger(__name__)


async def do_task(i):
    logger.info("start: %s", i)
    await asyncio.sleep(2)
    logger.info("end: %s", i)
    return i

semaphore = asyncio.Semaphore(3)

async def do_limitted_task(i):
    async with semaphore:
        return await do_task(i)


async def do_loop():
    futs = []
    nums = range(5)
    for i in nums:
        fut = asyncio.ensure_future(do_limitted_task(i))
        futs.append(fut)

    for i in asyncio.as_completed(futs):
        print(await i)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()

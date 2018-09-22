import time
import asyncio
import logging
logger = logging.getLogger(__name__)


def req(i):
    logger.info(f"{i}: start")
    time.sleep(0.5)
    logger.info(f"{i}: end")
    return i


async def req_async(sem, loop, i):
    async with sem:
        return await loop.run_in_executor(None, req, i)


async def run_async(loop):
    sem = asyncio.Semaphore(5)
    futs = [req_async(sem, loop, i) for i in range(13)]
    return await asyncio.gather(*futs, loop=loop)


logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run_async(loop)))

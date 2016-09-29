# -*- coding:utf-8 -*-
"""
task_a -> task_b
"""

import asyncio
import logging
logger = logging.getLogger(__name__)


a_semaphore = asyncio.Semaphore(3)
b_semaphore = asyncio.Semaphore(3)


async def do_task_a(i):
    logger.info("start a: %s", i)
    await asyncio.sleep(2)
    logger.info("end   a: %s", i)
    return i


async def do_task_b(i):
    logger.info("start b: %s", i)
    await asyncio.sleep(1)
    logger.info("end   b: %s", i)
    return i


async def do_limitted_task_a(i):
    async with a_semaphore:
        return await do_task_a(i)


async def do_limitted_task_b(i):
    async with b_semaphore:
        return await do_task_b(i)


async def do_loop():
    nums = range(5)
    a_futs = [do_limitted_task_a(i) for i in nums]
    b_futs = []

    for fut in asyncio.as_completed(a_futs):
        j = await fut
        b_futs.append(do_limitted_task_b(j))

    for fut in asyncio.as_completed(b_futs):
        print(await fut)


if __name__ == "__main__":
    import time
    st = time.time()
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()
    print(time.time() - st)

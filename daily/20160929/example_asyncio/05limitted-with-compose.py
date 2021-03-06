# -*- coding:utf-8 -*-

"""

WIP


numbers = [0, 1, 2, 3, 4]
taskA = 2s (limit = 3)
taskB = 1s (limit = 3)

total: 5s

final version::
(find-file "../../20161001/06limited-with-compose.py")
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
    finished = []
    f = loop.create_future()

    def consume_a(i):
        fut = asyncio.ensure_future(do_limitted_task_a(i))
        fut.add_done_callback(consume_b)

    def consume_b(_fut):
        j = _fut.result()
        fut = asyncio.ensure_future(do_limitted_task_b(j))
        fut.add_done_callback(finish)

    def finish(_fut):
        finished.append(_fut.result())
        if len(finished) == 5:
            f.set_result(finished)

    for i in nums:
        consume_a(i)
    await f


if __name__ == "__main__":
    import time
    st = time.time()
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()
    print(time.time() - st)

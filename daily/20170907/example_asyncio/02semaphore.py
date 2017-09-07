import time
import asyncio
import logging
logger = logging.getLogger("*")


def task(n):
    logger.info("before: %d", n)
    time.sleep(0.5)
    logger.info("after: %d", n)


async def limited_task(loop, sem, i):
    async with sem:
        await loop.run_in_executor(None, task, i)


async def do_task(loop):
    tasks = []
    sem = asyncio.Semaphore(5)
    for i in range(20):
        tasks.append(limited_task(loop, sem, i))
    await asyncio.gather(*tasks, loop=loop)


def main():
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_task(loop))
    print("end")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(message)s")
    st = time.time()
    main()
    print(time.time() - st)

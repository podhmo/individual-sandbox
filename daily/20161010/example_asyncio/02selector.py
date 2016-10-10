# -*- coding:utf-8 -*-
import logging
import asyncio
logger = logging.getLogger(__name__)

async def do_task(uid):
    d = 0.5
    logger.info("start %s (cost %s)", uid, d)
    await asyncio.sleep(d)
    logger.info("end %s", uid)
    return uid


class AsyncQueue:
    def __init__(self, N):
        self.N = N
        self.q = asyncio.Queue()
        self.semaphore = asyncio.Semaphore(N)

    @property
    def running(self):
        return self.N - self.semaphore._value

    def full(self):
        return self.semaphore._value <= 0

    async def put(self, c):
        if not asyncio.iscoroutine(c):
            raise TypeError("not coroutine: {}".format(c))
        await self.semaphore.acquire()

        def callback(fut):
            self.q.put_nowait(fut.result())
            self.semaphore.release()

        fut = asyncio.ensure_future(c)
        fut.add_done_callback(callback)
        return fut

    async def get(self):
        return await self.q.get()

    def empty(self):
        return self.q.empty() and self.semaphore._value == self.N

    def gettable(self):
        return not self.q.empty()


class Selector:
    def __init__(self, aq):
        self.aq = aq

    async def select(self, item):
        if not self.aq.full():
            return (None, await self.aq.put(item))
        else:
            return (item, await self.aq.get())

    async def put_untill_success(self, task):
        result_list = []
        while True:
            task, result = await self.select(task)
            if task is not None:
                result_list.append(result)
            else:
                return result_list


async def do_loop2():
    aq = AsyncQueue(2)
    selector = Selector(aq)
    for uid in range(5):
        logger.info("insert %s", uid)
        result = await selector.put_untill_success(do_task(uid))
        logger.info("gotcha %s", result)
    while not aq.empty():
        logger.info("gotcha %s", await aq.get())


if __name__ == "__main__":
    import time
    st = time.time()
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop2())
    logger.info("-- takes %s", time.time() - st)
    loop.close()

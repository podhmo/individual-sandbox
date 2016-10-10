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


async def do_loop():
    aq = AsyncQueue(2)
    nums = list(range(5))
    while nums or not aq.empty():
        if nums and not aq.full():
            uid = nums.pop()
            await aq.put(do_task(uid))
        else:
            uid = await aq.get()
            logger.info("gotcha %s", uid)


if __name__ == "__main__":
    import time
    st = time.time()
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    logger.info("-- takes %s", time.time() - st)
    loop.close()

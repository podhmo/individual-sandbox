import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


class Q:
    def __init__(self, q: asyncio.Queue):
        self.q = q
        self._users = 0
        self._canceled = False

    def use(self):
        self._users += 1

    async def cancel(self):
        if self._canceled:
            return
        self._canceled = True
        logger.debug("CANCEL	users=%s", self._users)
        for i in range(self._users):
            await self.q.put(None)

    def task_done(self):
        self.q.task_done()

    async def put(self, item):
        return await self.q.put(item)

    async def get(self):
        return await self.q.get()

    async def join(self):
        return await self.q.join()


class App:
    def __init__(self, loop: asyncio.BaseEventLoop):
        self.loop = loop

    async def generate(self, outq: Q):
        for i in range(10):
            logger.debug("generate[S]")
            await asyncio.sleep(0.1, loop=self.loop)
            logger.debug("generate[E]	v:%s", i)
            await outq.put(i)
        await outq.cancel()
        logger.debug("generate[CLOSE]")

    async def communicate(self, inq: Q, outq: Q, k: int):
        inq.use()

        while True:
            v = await inq.get()
            logger.debug("communicate[S,%s]	v:%s", k, v)
            if v is None:
                inq.task_done()
                break
            await asyncio.sleep(0.2, loop=self.loop)
            v = v * v
            logger.debug("communicate[E,%s]	v:%s", k, v)
            await outq.put(v)
            inq.task_done()
        await inq.join()
        await outq.cancel()
        logger.debug("communicate[CLOSE, %s]", k)

    async def aggregate(self, inq: Q):
        inq.use()

        while True:
            v = await inq.get()
            logger.debug("aggregate[S]	v:%s", v)
            if v is None:
                inq.task_done()
                break
            await asyncio.sleep(0.1, loop=self.loop)
            print(v)
            logger.debug("aggregate[E]	v:%s", v)
            inq.task_done()

        await inq.join()
        logger.debug("aggregate[CLOSE]")

    async def main(self):
        self.q0 = q0 = Q(asyncio.Queue())
        self.q1 = q1 = Q(asyncio.Queue())
        futs = []
        futs.append(self.loop.create_task(self.generate(q0)))
        futs.append(self.loop.create_task(self.communicate(q0, q1, k=0)))
        futs.append(self.loop.create_task(self.communicate(q0, q1, k=1)))
        futs.append(self.loop.create_task(self.communicate(q0, q1, k=2)))
        futs.append(self.loop.create_task(self.aggregate(q1)))
        await asyncio.wait(futs, loop=self.loop)


# import signal

# def handler(signum, tb):
#     import pdb
#     pdb.set_trace()

# signal.signal(signal.SIGINT, handler)
app = App(asyncio.get_event_loop())
app.loop.run_until_complete(app.main())

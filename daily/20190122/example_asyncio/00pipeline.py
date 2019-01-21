import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


class App:
    def __init__(self, loop: asyncio.BaseEventLoop):
        self.loop = loop

    async def generate(self, outq: asyncio.Queue):
        for i in range(10):
            logger.debug("generate[S]")
            await asyncio.sleep(0.1, loop=self.loop)
            logger.debug("generate[E]	v:%s", i)
            await outq.put(i)
        await outq.put(None)
        logger.debug("generate[CLOSE]")

    async def communicate(self, inq: asyncio.Queue, outq: asyncio.Queue):
        while True:
            v = await inq.get()
            logger.debug("communicate[S]	v:%s", v)
            if v is None:
                inq.task_done()
                break
            await asyncio.sleep(0.1, loop=self.loop)
            v = v * v
            logger.debug("communicate[E]	v:%s", v)
            await outq.put(v)
            inq.task_done()
        await inq.join()
        await outq.put(None)
        logger.debug("communicate[CLOSE]")

    async def aggregate(self, inq: asyncio.Queue):
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
        q0 = asyncio.Queue()
        q1 = asyncio.Queue()
        futs = []
        futs.append(self.loop.create_task(self.generate(q0)))
        futs.append(self.loop.create_task(self.communicate(q0, q1)))
        futs.append(self.loop.create_task(self.aggregate(q1)))
        await asyncio.wait(futs, loop=self.loop)


app = App(asyncio.get_event_loop())
app.loop.run_until_complete(app.main())

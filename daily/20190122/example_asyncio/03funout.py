import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


# provider,communicator,consumer
class Provider:
    def __init__(self, loop: asyncio.BaseEventLoop):
        self.loop = loop

    async def __call__(self, outq: asyncio.Queue):
        for i in range(10):
            logger.debug("provide[S]")
            await asyncio.sleep(0.1, loop=loop)
            logger.debug("provide[E]	v:%s", i)
            await outq.put(i)
        await outq.put(None)
        logger.debug("provide[CLOSE]")


class Communicator:
    def __init__(self, loop: asyncio.BaseEventLoop):
        self.loop = loop

    async def __call__(self, inq: asyncio.Queue, outq: asyncio.Queue):
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


class Aggregator:
    def __init__(self, loop: asyncio.BaseEventLoop):
        self.loop = loop

    async def __call__(self, inq: asyncio.Queue):
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


async def main(loop):
    q0 = asyncio.Queue()
    q1 = asyncio.Queue()

    provider = Provider(loop)
    communicator = Communicator(loop)
    aggregator = Aggregator(loop)

    futs = []
    futs.append(provider(q0))
    futs.append(communicator(q0, q1))
    futs.append(aggregator(q1))
    await asyncio.wait(futs, loop=loop)


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))

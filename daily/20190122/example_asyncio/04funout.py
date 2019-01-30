import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


def from_iterable(itr):
    async def call(loop, outq: asyncio.Queue):
        for i in range(10):
            logger.debug("provide[S]	v:%s", i)
            await outq.put(i)
        await outq.put(None)
        logger.debug("provide[CLOSE]")

    return call


def communicate(afn):
    async def call(loop, inq: asyncio.Queue, outq: asyncio.Queue):
        while True:
            v = await inq.get()
            logger.debug("communicate[S]	v:%s", v)
            if v is None:
                inq.task_done()
                break
            v = await afn(v)
            logger.debug("communicate[E]	v:%s", v)
            await outq.put(v)
            inq.task_done()
        await inq.join()
        await outq.put(None)
        logger.debug("communicate[CLOSE]")

    return call


def consume(afn):
    async def call(loop, inq: asyncio.Queue):
        while True:
            v = await inq.get()
            logger.debug("consume[S]	v:%s", v)
            if v is None:
                inq.task_done()
                break
            v = await afn(v)
            logger.debug("consume[E]	v:%s", v)
            inq.task_done()
        await inq.join()
        logger.debug("consume[CLOSE]")

    return call


async def main(loop):
    q0 = asyncio.Queue()
    q1 = asyncio.Queue()

    async def transform(v):
        await asyncio.sleep(0.1, loop=loop)
        return v * v

    async def write(v):
        print(v)

    futs = [
        from_iterable(range(10))(loop, q0),
        communicate(transform)(loop, q0, q1),
        consume(write)(loop, q1),
    ]
    await asyncio.wait(futs, loop=loop)


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))

import asyncio
import logging
logger = logging.getLogger(__name__)


async def provide(loop, n):
    for i in range(n):
        logger.info("provide	v:%s", i)
        yield i
        await asyncio.sleep(0.1, loop=loop)


async def communicate(loop, aitr):
    async for i in aitr:
        logger.info("communicate	v:%s", i)
        yield i * i
        await asyncio.sleep(0.1, loop=loop)


async def consume(loop, aitr, k):
    while True:
        i = await aitr.__anext__()
        logger.info("consume[%s]	v:%s", k, i)
        print([k, i])
        await asyncio.sleep(0.1, loop=loop)


async def main(loop):
    aitr = provide(loop, 10)
    aitr = communicate(loop, aitr)
    x = consume(loop, aitr, "0")
    y = consume(loop, aitr, "1")
    await asyncio.gather(x, y, loop=loop)


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))

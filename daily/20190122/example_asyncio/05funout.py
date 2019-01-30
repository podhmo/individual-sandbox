import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


class r:
    pass


@r.provide
async def provide(aitr):
    for i in range(10):
        yield i


@r.communicate
async def communicate(v):
    await asyncio.sleep(0.1, loop=loop)
    return v * v


@r.aggregate
async def write(v):
    print(v)


async def main(loop):
    q0 = asyncio.Queue()
    q1 = asyncio.Queue()

    futs = [
        provide(q0),
        communicate(q0, q1),
        write(q1),
    ]
    await asyncio.wait(futs, loop=loop)


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))

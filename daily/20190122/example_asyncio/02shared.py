import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


def shared(loop: asyncio.BaseEventLoop):
    fut = loop.create_future()

    def callback():
        print("called")
        fut.set_result("xxx")

    loop.call_later(0.2, callback)
    return fut


async def worker(loop, k, fut):
    for i in range(3):
        print(i, k)
        asyncio.sleep(0.3, loop=loop)
    print("finish....", i, await fut)


async def main(loop):
    sentinel = shared(loop)
    futs = [
        worker(loop, 0, sentinel),
        worker(loop, 1, sentinel),
    ]
    await asyncio.wait(futs, loop=loop)


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))

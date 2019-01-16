import time
import asyncio
import logging

logging.basicConfig(level=logging.DEBUG)


async def hello(name: str):
    t = time.time()
    print(name, "hello")
    await asyncio.sleep(1)
    print(name, "bye", time.time() - t)


async def run():
    futs = [
        hello("foo"),
        hello("bye"),
    ]
    await asyncio.wait(futs)


loop = asyncio.get_event_loop()
loop.run_until_complete(run())
loop.close()

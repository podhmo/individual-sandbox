import asyncio
from aioknife.synclike import Group
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))

loop = asyncio.get_event_loop()


async def run(i):
    print("before", i)
    await asyncio.sleep(0.5, loop=loop)
    print("after", i)
    return i


async def main():
    g = Group(loop=loop)
    for i in range(10):
        g.go(run, i)
    print("R", await g.execute())


loop.run_until_complete(main())

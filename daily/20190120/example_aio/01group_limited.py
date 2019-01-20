import asyncio
from aioknife.synclike import Group
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


async def run(i):
    print("before", i)
    await asyncio.sleep(0.5)
    print("after", i)
    return i


g = Group(concurrency=5)
for i in range(10):
    g.go(run, i)
print(g.wait())

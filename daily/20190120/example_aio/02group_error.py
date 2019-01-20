import asyncio
from aioknife.synclike import Group
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


async def run(i):
    print("before", i)
    await asyncio.sleep(0.5)
    if i == 8:
        raise Exception("hmm")
    print("after", i)
    return i


g = Group()
for i in range(10):
    g.go(run, i)
try:
    print(g.wait())
except Exception as e:
    print(e)

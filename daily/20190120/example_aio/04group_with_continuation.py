import asyncio
from aioknife.synclike import Group
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


async def run(i):
    print("before", i)
    await asyncio.sleep(0.5)
    if i == 2:
        raise Exception("hmm")
    print("after", i)
    return i


def callback(fut):
    if fut.exception() is not None:
        print("E", fut.exception())
    else:
        print("R", fut.result())


g = Group(concurrency=2)
for i in range(10):
    fut = g.go(run, i)
    fut.add_done_callback(callback)

try:
    print(g.wait())
except Exception as e:
    print("!!!", e)

import os
import asyncio
import logging
from aioknife.synclike import Executor
logger = logging.getLogger(__name__)


async def run(i):
    logger.info("before %s", i)
    await asyncio.sleep(0.05)
    logger.info("after %s", i)
    return i


logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


def callback(fut):
    print("!!", fut.result())


futs = []
with Executor() as submit:
    for i in range(3):
        fut = submit(run, i)
        futs.append(fut)
        fut.add_done_callback(callback)

with Executor(callback=callback) as submit:
    for i in range(3):
        submit(run, i)

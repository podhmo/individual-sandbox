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
with Executor() as submit:
    submit(run, 0)
    submit(run, 1)
    submit(run, 2)

    # def do():
    #     print("hai")
    # submit(do)

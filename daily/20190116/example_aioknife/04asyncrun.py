import os
import asyncio
import logging
from aioknife.synclike import Executor
logger = logging.getLogger(__name__)


async def run(i):
    logger.info("before %s", i)
    await asyncio.sleep(0.05)
    if i == 2:
        raise Exception("hmm")
    logger.info("after %s", i)
    return i


async def main(*, return_exceptions=False):
    ex = Executor()
    ex.register(run, 0)
    ex.register(run, 1)
    ex.register(run, 2)
    return await ex.execute(return_exceptions=return_exceptions)


import asyncio
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
loop = asyncio.get_event_loop()
try:
    print("!!", loop.run_until_complete(main()))
except Exception as e:
    print("@", e)

print("!!!!", loop.run_until_complete(main(return_exceptions=True)))

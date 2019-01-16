import os
import asyncio
import logging
from aioknife.synclike import Executor

logger = logging.getLogger(__name__)


async def run(i, *, level=0):
    logger.info("before %s, level=%s", i, level)
    await asyncio.sleep(0.05)
    logger.info("after %s, level=%s", i, level)

    if i == 2 and level < 3:

        async def cont():
            return await run(i, level=level + 1)

        return cont
    return i


logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
with Executor() as submit:
    submit(run, 0)
    submit(run, 1)
    submit(run, 2)

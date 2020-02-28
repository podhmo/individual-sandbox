import sys
import typing as t
import asyncio
import random
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)


async def update_check() -> t.Awaitable[t.Optional[str]]:
    logger.info("-> update_check ...")
    await asyncio.sleep(0.5)
    logger.info("<- ... update_check")

    if random.random() < 0.2:
        # update is not found
        return None

    # update is found
    return "0.8.8"


def run_main():
    import time

    logger.info("do something (main)")
    for i in range(6):
        logger.info(".")
        time.sleep(0.1)
    logger.info("ok")


@as_command
def main():
    async def run():
        fut = asyncio.ensure_future(update_check())
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, run_main)
        update_version = await fut
        if update_version is not None:
            logger.info(f"A new release of gh is available: xxx → {update_version}")

    asyncio.run(run())

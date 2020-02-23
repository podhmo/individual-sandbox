import asyncio
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)


@as_command
def run():
    async def run():
        logger.info("hello ...")
        await asyncio.sleep(0.1)
        logger.info("... world")

    asyncio.get_event_loop().run_until_complete(run())

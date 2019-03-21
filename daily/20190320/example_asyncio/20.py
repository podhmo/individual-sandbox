import logging
import asyncio
import time
from functools import partial
from mysleep import mysleep

logger = logging.getLogger(__name__)


async def run():
    loop = asyncio.get_event_loop()
    st = time.time()
    logger.info("**start**")
    result = await asyncio.gather(
        *[
            loop.run_in_executor(None, partial(mysleep, "x", 1)),
            loop.run_in_executor(None, partial(mysleep, "y", 1)),
            loop.run_in_executor(None, partial(mysleep, "z", 2)),
        ]
    )
    logger.info("**end** %r %r", result, time.time() - st)


logging.basicConfig(level=logging.DEBUG, format="%(asctime)s" + logging.BASIC_FORMAT)
asyncio.run(run(), debug=True)

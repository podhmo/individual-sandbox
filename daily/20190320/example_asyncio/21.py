import logging
import asyncio
import time
from concurrent.futures import ProcessPoolExecutor
from functools import partial
from mysleep import mysleep

logger = logging.getLogger(__name__)


async def run():
    loop = asyncio.get_event_loop()
    st = time.time()
    logger.info("**start**")
    executor = ProcessPoolExecutor(max_workers=3)
    result = await asyncio.gather(
        *[
            loop.run_in_executor(executor, partial(mysleep, "x", 1)),
            loop.run_in_executor(executor, partial(mysleep, "y", 1)),
            loop.run_in_executor(executor, partial(mysleep, "z", 2)),
        ]
    )
    logger.info("**end** %r %r", result, time.time() - st)


logging.basicConfig(level=logging.DEBUG, format="%(asctime)s" + logging.BASIC_FORMAT)
asyncio.run(run(), debug=True)

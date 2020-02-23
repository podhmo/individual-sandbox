import typing as t
import asyncio
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)


async def arange(n: int) -> t.AsyncIterator[int]:
    for i in range(n):
        yield i
        await asyncio.sleep(0.1)


@as_command  # type:ignore
def run() -> None:
    async def run() -> None:
        async for i in arange(5):
            logger.info(i)

    asyncio.get_event_loop().run_until_complete(run())

import logging
import typing as t
import asyncio
from functools import partial
from handofcats import as_command

logger = logging.getLogger(__name__)


class AExecutor:
    def __init__(self):
        self.q = asyncio.Queue()

    async def __aenter__(self) -> t.Callable[..., t.Awaitable[None]]:
        async def loop():
            while True:
                afn = await self.q.get()
                if afn is None:
                    self.q.task_done()
                    break
                await afn()
                self.q.task_done()

        asyncio.ensure_future(loop())
        return self.q.put

    async def __aexit__(
        self,
        exc: t.Optional[t.Type[BaseException]],
        value: t.Optional[BaseException],
        tb: t.Any,
    ) -> None:
        await self.q.put(None)
        await self.q.join()


async def arange(n: int) -> t.AsyncIterator[int]:
    for i in range(n):
        yield i
        await asyncio.sleep(0.1)


async def run():
    async with AExecutor() as submit:

        async def task(tag: str):
            async for i in arange(3):
                logger.info("%s:%d", tag, i)

        await submit(partial(task, "x"))
        await submit(partial(task, "y"))


@as_command
def main():
    asyncio.get_event_loop().run_until_complete(run())

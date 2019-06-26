import typing as t
import logging
import contextlib
import asyncio
import signal
from functools import partial
from asyncio.subprocess import Process
from asyncio.unix_events import (
    SelectorEventLoop,
)  # unix only? (see asyncio.unix_events)

logger = logging.getLogger(__name__)


class Supervisor:
    def __init__(self, *, loop: SelectorEventLoop) -> None:
        self.ps: t.List[Process] = []
        self.loop = loop
        self.on_init()  # xxx:

    def on_init(self) -> None:
        self.loop.add_signal_handler(
            signal.SIGINT, partial(self.send_signal, signal.SIGINT)
        )

    def watch(self, p: Process) -> None:
        self.ps.append(p)

    def send_signal(self, sig):
        for p in self.ps:
            # todo: handling process lookup error?
            try:
                p.send_signal(sig)
            except ProcessLookupError:
                logger.debug("send: %s", sig, exc_info=True)
                pass

    def terminate(self) -> None:
        for p in self.ps:
            p.terminate()

    def kill(self) -> None:
        for p in self.ps:
            p.kill()


async def queue_as_aiter(q):
    canceled_exc = None
    while True:
        try:
            item = await q.get()
            yield item
        except asyncio.CancelledError as e:
            canceled_exc = e
            break
        finally:
            if canceled_exc is None:
                q.task_done()


@contextlib.asynccontextmanager
async def consuming(q, ause):
    async def asend(aiter):
        async for item in aiter:
            await q.put(item)

    try:
        loop = asyncio.get_event_loop()
        ct = loop.create_task(ause(queue_as_aiter(q)))
        yield asend
        await q.join()
        assert q.empty()
    finally:
        ct.cancel()

import typing as t
import asyncio
import logging
import sys
import signal
from functools import partial
from asyncio.queues import Queue
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


class Feeder:
    def __init__(
        self, q: asyncio.Queue, *, loop: asyncio.BaseEventLoop, encoding="utf-8"
    ) -> None:
        self.q = q
        self.loop = loop
        self.encoding = encoding
        self.futs = []

    def feed(self, p: Process, *, uid):
        self.futs.append(self.loop.create_task(self._feed(p, uid=uid)))

    async def _feed(self, p: Process, *, uid):
        async for data in p.stdout:
            await self.q.put((uid, p.pid, data.decode(self.encoding)))
        # todo: stderr?


class Consumer:
    def __init__(self, q: asyncio.Queue, *, loop: asyncio.BaseEventLoop) -> None:
        self.q = q
        self.loop = loop
        self.running = True
        self.futs = []

    async def wait(self):
        await self.q.join()
        assert self.q.empty()
        self.running = False  # xxx

    def consume(self) -> None:
        self.futs.append(self.loop.create_task(self._consume()))

    async def _consume(self):
        # todo: handling exception?
        while self.running:
            try:
                (uid, pid, line) = await self.q.get()
                print(f"\x1b[1;7;{uid+91}m{pid}\x1b[0m: {line.rstrip()}")
            except asyncio.CancelledError:
                assert self.q.empty
            else:
                self.q.task_done()


async def main():
    loop = asyncio.get_event_loop()
    q = Queue()

    feeder = Feeder(q, loop=loop)
    suprvisor = Supervisor(loop=loop)
    consumer = Consumer(q, loop=loop)  # todo: be async iterator

    async def spawn(uid) -> Process:
        code = "import time; [(print(i) or time.sleep(0.3)) for i in range(10)]"

        p = await asyncio.create_subprocess_exec(
            sys.executable, "-u", "-c", code, stdout=asyncio.subprocess.PIPE,
        )
        suprvisor.watch(p)
        feeder.feed(p, uid=uid)
        await p.wait()
        return p

    consumer.consume()  # todo: async context manager
    await asyncio.wait([spawn(i) for i in range(7)])

    print("end..")
    await consumer.wait()
    print("end....")


asyncio.run(main(), debug=True)

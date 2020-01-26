import asyncio
import logging
import itertools
import contextlib
from collections import defaultdict
from functools import partial, cached_property
from handofcats import as_command

logger = logging.getLogger(__name__)


class FakeSender:
    def __init__(self, ipc, endpoint: str, *, uid: int):
        self.ipc = ipc
        self.endpoint = endpoint
        self.q = self.ipc.mapping[endpoint]
        self.uid = uid

    async def send(self, name, *, args=None, kwargs=None):
        item = {"name": name, "args": args, "kwargs": kwargs}
        logger.info("send: %r	uid:%d", item, self.uid)
        return self.q.put_nowait(item)


class FakeReciver:
    def __init__(self, ipc, endpoint: str, *, uid: int):
        self.ipc = ipc
        self.endpoint = endpoint
        self.q = self.ipc.mapping[endpoint]
        self.uid = uid

    async def recv(self):
        item = await self.q.get()
        logger.info("recv: %r	uid:%d", item, self.uid)
        return item, self.q.task_done


class FakeIPC:
    def __init__(self, mapping, *, ev: asyncio.Event, c):
        self.mapping = mapping
        self.ev = ev
        self.c = c

    @contextlib.asynccontextmanager
    async def serve(self, endpoint: str):
        sender = FakeSender(self, endpoint, uid=next(self.c))
        yield sender
        self.ev.set()

    @contextlib.asynccontextmanager
    async def connect(self, endpoint: str):
        yield FakeReciver(self, endpoint, uid=next(self.c))


class Minitask:
    def __init__(self):
        self.mapping = defaultdict(asyncio.Queue)
        self.c = itertools.count()

    def IPC(self):
        return FakeIPC(self.mapping, ev=self.ev, c=self.c)

    @cached_property
    def ev(self):
        return asyncio.Event()

    async def wait(self, endpoint: str, *, n: int):
        await self.ev.wait()
        q = self.mapping[endpoint]
        for i in range(n):
            q.put_nowait(None)
        await q.join()


minitask = Minitask()


async def producer(*, endpoint: str):
    ipc = minitask.IPC()
    async with ipc.serve(endpoint) as x:
        for i in range(5):
            # msgpackrpc not support kwargs
            await x.send("say", kwargs={"message": "hello", "i": i})
            await asyncio.sleep(0.1)


async def consumer(*, endpoint: str):
    ipc = minitask.IPC()
    async with ipc.connect(endpoint) as x:
        while True:
            item, task_done = await x.recv()
            if item is None:
                task_done()
                break
            await asyncio.sleep(0.2)
            task_done()


@as_command
def run():
    async def _run():
        futs = []
        endpoint = "xxx"
        futs.append(asyncio.ensure_future(producer(endpoint=endpoint)))

        n = 2
        for i in range(n):
            futs.append(asyncio.ensure_future(consumer(endpoint=endpoint)))

        await minitask.wait(endpoint, n=n)

        for fut in futs:
            fut.result()
        print("ok")

    asyncio.run(_run(), debug=True)

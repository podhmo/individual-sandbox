import subprocess
import random
import asyncio
import signal
import logging
from functools import partial
from collections import namedtuple


logger = logging.getLogger(__name__)
StepContext = namedtuple("StepContext", "coro_fn, step, loop, supervisor")
Item = namedtuple("Item", "coro_fn, step, value")


class Supervisor:
    def __init__(self, maxsize=0, loop=None):
        self.loop = loop or asyncio.get_event_loop()
        self.is_running = False
        self.async_tasks = set()
        self.q = asyncio.Queue(maxsize=maxsize)

    def setup(self, signums=[signal.SIGINT, signal.SIGHUP]):
        for s in signums:
            signal.signal(s, self.on_interrupt)

    async def teardown(self):
        logger.info("graceful stop")
        await asyncio.gather(*self.async_tasks)

    def on_finished(self):
        logger.info("finished")

    def on_interrupt(self, signum, frame):
        self.is_running = False
        logger.info("interrupt!!")

    def on_step(self, fut, scontext):
        # default implementation
        assert scontext.loop == self.loop
        assert scontext.supervisor == self
        assert scontext.step == self.on_step
        value = fut.result()
        item = Item(coro_fn=scontext.coro_fn, step=scontext.step, value=value)
        self.q.put_nowait(item)

    def consume(self, item):
        coro = item.coro_fn(item.value)
        fut = asyncio.ensure_future(coro)
        self.async_tasks.add(fut)
        fut.add_done_callback(lambda fut: self.async_tasks.discard(fut))
        if self.is_running:
            scontext = StepContext(coro_fn=item.coro_fn, step=item.step, loop=self.loop, supervisor=self)
            fut.add_done_callback(partial(item.step, scontext=scontext))

    def register(self, coro_fn, value=None, step=None):
        step = step or self.on_step
        item = Item(coro_fn=coro_fn, step=step, value=value)
        self.q.put_nowait(item)

    async def run_loop(self, loop=None):
        self.is_running = True
        while self.is_running:
            item = await self.q.get()
            self.consume(item)
        await self.teardown()


async def action(i, uid="(uid)"):
    wait = random.random() * 3.0
    logger.info("start uid=%s, i=%d, wait=%f", uid, i, wait)
    await asyncio.sleep(wait)
    logger.info("end uid=%s, i=%d", uid, i)
    return i + 1

async def ls(i, uid="(uid)"):
    logger.info("start ls uid=%s, i=%d, wait=%f", uid, i, 2)
    p = await asyncio.create_subprocess_shell("sleep 2 && ls", stdout=subprocess.PIPE)
    logger.info(await p.stdout.read())
    await p.wait()
    logger.info("end uid=%s, i=%d", uid, i)
    return i + 1


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()

    supervisor = Supervisor(loop=loop)
    supervisor.register(partial(action, uid="A"), 1)
    supervisor.register(partial(action, uid="B"), 1)
    supervisor.register(partial(action, uid="C"), 1)
    supervisor.register(partial(action, uid="D"), 1)
    supervisor.register(partial(action, uid="E"), 1)
    supervisor.register(partial(ls, uid="E"), 1)

    supervisor.setup([signal.SIGINT])
    loop.run_until_complete(supervisor.run_loop())
    loop.close()

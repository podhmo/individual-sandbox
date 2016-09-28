import sys
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


async def spawn_process(i, uid="(uid)", cmd="", waittime=0):
    logger.info("init name=%r, cmd=%r", uid, cmd)
    p = await asyncio.create_subprocess_shell(cmd, stderr=sys.stderr, stdout=sys.stdout)
    logger.info("process started pid=%s, name=%r", p.pid, uid)
    await p.wait()
    if not p.returncode:
        logger.info("process ok pid=%s, name=%r", p.pid, uid)
    else:
        logger.info("process ng returncode=%s, pid=%s, name=%r", p.returncode, p.pid, uid)
        logger.info("wait: name=%r, waittime=%s", uid, waittime)
        await asyncio.sleep(waittime)
    return i + 1

async def run_python(i, uid="(uid)", code="", waittime=0):
    cmd = "{} -c '{}'".format(sys.executable, code)
    return await spawn_process(i, uid=uid, cmd=cmd, waittime=waittime)

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()

    supervisor = Supervisor(loop=loop)
    supervisor.register(partial(run_python, uid="A", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.register(partial(run_python, uid="B", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.register(partial(run_python, uid="C", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.register(partial(run_python, uid="D", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.register(partial(run_python, uid="E", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.register(partial(run_python, uid="F", waittime=2, code="import random; import sys; s = int(random.random() > 0.5); print(s); sys.exit(s)"), 1)
    supervisor.setup([signal.SIGINT])
    loop.run_until_complete(supervisor.run_loop())
    loop.close()

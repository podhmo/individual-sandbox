import signal
import asyncio
import logging
import contextlib
import os

logger = logging.getLogger(__name__)


# 丁寧に書くならloopを明示的に(libraryならloopを明示的にわたす)
# 雑ならloopは暗黙的で(appで依存の終端なら暗黙的で十分)
# 今回はappのつもり

# for testing: (don't use in production)
is_ng = os.environ.get("NG", "") != ""


def _do():
    logger.info("start")
    import time

    time.sleep(0.5)
    logger.info("end")


async def do_task():
    for i in range(10):
        logger.info("running task %s", i)
        await asyncio.sleep(0.5)

    if is_ng:
        raise Exception("hmm")
    return "ok"


@contextlib.contextmanager
def spawn_task_scope(loop):
    futs = set()

    def _callback(fut):
        futs.remove(fut)
        fut.cancelled() or fut.result()

    def spawn(fn):
        fut = asyncio.ensure_future(loop.run_in_executor(None, fn), loop=loop)
        futs.add(fut)
        fut.add_done_callback(_callback)

    try:
        yield spawn
    finally:
        for fut in futs:
            fut.cancel()


async def update_lifetime(
    loop: asyncio.BaseEventLoop, ev: asyncio.Event, lifetime: int
):
    with spawn_task_scope(loop) as spawn:
        i = 0
        while not ev.is_set():
            logger.info("update lifetime:%d %d", i, lifetime + 20)  # xxx: +20?
            spawn(_do)
            await asyncio.sleep(1)
            i += 1


async def run(*, loop: asyncio.BaseEventLoop = None):
    loop = loop or asyncio.get_event_loop()

    ev = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev.set)

    update_task = loop.create_task(update_lifetime(loop, ev, lifetime=3))
    update_task.add_done_callback(lambda fut: fut.cancelled() or fut.result())
    atask = loop.create_task(do_task())
    done, pending = await asyncio.wait(
        [ev.wait(), atask], return_when=asyncio.FIRST_COMPLETED
    )

    result = None
    if atask in done:
        try:
            result = await atask
        except Exception as e:
            result = f"ng ({e!r})"
        update_task.cancel()

    interrupted = ev.is_set()
    if interrupted:
        logger.info("task is interrupted (catch SIGINT)")
    else:
        logger.info("task completed, result=%r", result)


logging.basicConfig(level=logging.DEBUG, format="%(asctime)s " + logging.BASIC_FORMAT)
asyncio.run(run(), debug=True)

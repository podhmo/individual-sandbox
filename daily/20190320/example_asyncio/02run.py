import signal
import asyncio
import logging
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


async def update_lifetime(
    loop: asyncio.BaseEventLoop, ev: asyncio.Event, lifetime: int
):
    i = 0
    while not ev.is_set():
        logger.info("update lifetime:%d %d", i, lifetime + 20)  # xxx: +20?
        await asyncio.wait([loop.run_in_executor(None, _do), asyncio.sleep(1)])
        i += 1


async def run(*, loop: asyncio.BaseEventLoop = None):
    loop = loop or asyncio.get_event_loop()

    ev = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev.set)

    atask = loop.create_task(do_task())
    done, pending = await asyncio.wait(
        [update_lifetime(loop, ev, lifetime=3), atask],
        return_when=asyncio.FIRST_COMPLETED,
    )

    result = None
    if atask in done:
        try:
            result = await atask
        except Exception as e:
            result = f"ng ({e!r})"

    interrupted = ev.is_set()
    if interrupted:
        logger.info("task is interrupted (catch SIGINT)")
    else:
        logger.info("task completed, result=%r", result)
    ev.set()
    await asyncio.wait(pending)


logging.basicConfig(level=logging.DEBUG, format="%(asctime)s " + logging.BASIC_FORMAT)
asyncio.run(run(), debug=True)

import signal
import asyncio
import logging
import os
import requests
from mysleep import mysleep
from concurrent.futures import (
    ProcessPoolExecutor,
    TimeoutError,
    CancelledError,
    ThreadPoolExecutor,
)


logger = logging.getLogger(__name__)


# for testing: (don't use in production)
is_ng = os.environ.get("NG", "") != ""


def task():
    return mysleep("hello", 8)


async def do_task(loop):
    logger.info("running task")
    executor = ProcessPoolExecutor(max_workers=1)
    print(
        "****************************************",
        await loop.run_in_executor(executor, task),
        "****************************************",
    )
    logger.info("end task")
    if is_ng:
        raise Exception("hmm")
    return "ok"


async def update_lifetime(ev: asyncio.Event, lifetime: int):
    i = 0
    while not ev.is_set():
        logger.info("update lifetime:%d %d", i, lifetime + 20)  # xxx: +20?
        await asyncio.sleep(1)
        i += 1


async def run(*, loop: asyncio.BaseEventLoop = None):
    loop = loop or asyncio.get_event_loop()

    ev = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev.set)
    loop.add_signal_handler(signal.SIGTERM, ev.set)

    update_task = loop.create_task(update_lifetime(ev, lifetime=3))

    atask = loop.create_task(do_task(loop))
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
        logger.info("** task is interrupted (catch SIGINT) **")
    else:
        logger.info("** task completed, result=%r **", result)


logging.basicConfig(level=logging.DEBUG, format=f"%(asctime)s: {logging.BASIC_FORMAT}")
asyncio.run(run(), debug=True)

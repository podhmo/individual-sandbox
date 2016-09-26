import asyncio
import random
import logging
import signal
logger = logging.getLogger(__name__)


@asyncio.coroutine
def action(uid, i=0):
    wait = random.random() * 3.0
    logger.info("start uid=%s, i=%d, wait=%f", uid, i, wait)
    yield from asyncio.sleep(wait)
    logger.info("end uid=%s, i=%d", uid, i)
    return lambda: action(uid, i + 1)


@asyncio.coroutine
def do_loop():
    q = asyncio.Queue()
    ws = set()
    is_running = True

    def on_interrupt(signum, frame):
        nonlocal is_running
        is_running = False
        logger.info("interrupt!!")

    def consume(fn):
        c = fn()
        fut = asyncio.ensure_future(c)
        ws.add(fut)
        fut.add_done_callback(lambda fut: ws.discard(fut))
        if is_running:
            fut.add_done_callback(lambda fut: q.put_nowait(fut.result()))

    signal.signal(signal.SIGTERM, on_interrupt)
    signal.signal(signal.SIGINT, on_interrupt)
    signal.signal(signal.SIGQUIT, on_interrupt)
    signal.signal(signal.SIGHUP, on_interrupt)

    q.put_nowait(lambda: action("A"))
    q.put_nowait(lambda: action("B"))
    q.put_nowait(lambda: action("C"))
    q.put_nowait(lambda: action("D"))
    q.put_nowait(lambda: action("E"))

    while is_running:
        c = yield from q.get()
        consume(c)

    logger.info("graceful stop")
    yield from asyncio.gather(*ws)
    print("yay")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())

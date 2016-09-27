import asyncio
import random
import logging
logger = logging.getLogger(__name__)


@asyncio.coroutine
def action(uid, i=0):
    wait = random.random() * 3.0
    logger.info("start uid=%f, i=%d, wait=%f", uid, i, wait)
    yield from asyncio.sleep(wait)
    logger.info("end uid=%s, i=%d", uid, i)
    return lambda: action(uid, i + 1)


@asyncio.coroutine
def do_loop():
    q = asyncio.Queue()

    def consume(fn):
        c = fn()
        fut = asyncio.ensure_future(c)
        fut.add_done_callback(lambda fut: q.put_nowait(fut.result()))

    q.put_nowait(lambda: action(random.random()))
    q.put_nowait(lambda: action(random.random()))
    q.put_nowait(lambda: action(random.random()))
    q.put_nowait(lambda: action(random.random()))
    q.put_nowait(lambda: action(random.random()))

    while True:
        c = yield from q.get()
        consume(c)

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(levelname)s\t%(asctime)s\t%(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()

import asyncio
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)


async def producer(*, q: asyncio.Queue, ev: asyncio.Event, uid: int):
    for i in range(5):
        item = {"method": "say", "kwargs": {"message": "hello", "i": i}}
        logger.info("send: %r	uid:%d", item, uid)
        await q.put(item)
        await asyncio.sleep(0.1)
    ev.set()


async def consumer(*, q: asyncio.Queue, uid: int):
    while True:
        item = await q.get()
        if item is None:
            q.task_done()
            break
        logger.info("recv: %r	uid:%d", item, uid)
        await asyncio.sleep(0.2)
        q.task_done()


@as_command
def run():
    import itertools

    c = itertools.count()

    async def _run():
        q = asyncio.Queue()
        producer_end = asyncio.Event()
        futs = []
        futs.append(asyncio.ensure_future(producer(q=q, ev=producer_end, uid=next(c))))

        n = 2
        for i in range(n):
            futs.append(asyncio.ensure_future(consumer(q=q, uid=next(c))))
        await producer_end.wait()
        for i in range(n):
            q.put_nowait(None)
        await q.join()

        for fut in futs:
            fut.result()
        print("ok")

    asyncio.run(_run(), debug=True)

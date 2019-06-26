import asyncio
import contextlib


async def queue_as_aiter(q):
    canceled_exc = None
    while True:
        try:
            item = await q.get()
            yield item
        except asyncio.CancelledError as e:
            canceled_exc = e
            break
        finally:
            if canceled_exc is None:
                q.task_done()


@contextlib.asynccontextmanager
async def consuming(q, ause):
    async def asend(aiter):
        async for item in aiter:
            await q.put(item)

    try:
        loop = asyncio.get_event_loop()
        ct = loop.create_task(ause(queue_as_aiter(q)))
        yield asend
        await q.join()
        assert q.empty()
    finally:
        ct.cancel()


async def run():
    async def producer(*, uid):
        async for i in range(10):
            yield (uid, i)
            await asyncio.sleep(0.2)

    async def consumer(aiter):
        async for (uid, i) in aiter:
            print(f"{uid}: {i}")

    q = asyncio.Queue()
    async with consuming(q, consumer) as asend:
        await asyncio.wait([asend(producer(uid=i)) for i in range(2)])


asyncio.run(run(), debug=True)

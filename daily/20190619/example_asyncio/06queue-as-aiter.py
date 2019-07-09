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
    # producer x 2
    # consumer x 1

    async def producer(k, d):
        for i in range(5):
            print("\t->", k, i)
            yield (k, i)
            await asyncio.sleep(d)

    async def consumer(aiter):
        async for (k, i) in aiter:
            print("<-", k, i)

    q = asyncio.Queue()
    async with consuming(q, consumer) as asend:
        futs = []
        for i in range(2):
            d = 0.3 + (0.1 * i)
            futs.append(asend(producer(i, d=d)))
        await asyncio.wait(futs)


async def shutdown_check():
    current_task = asyncio.current_task()
    print([t for t in asyncio.all_tasks() if t != current_task])


asyncio.run(run(), debug=True)
asyncio.run(shutdown_check(), debug=True)
# -- stdout --------------------
# >> 	-> 1 0
# >> 	-> 0 0
# >> <- 1 0
# >> <- 0 0
# >> 	-> 0 1
# >> <- 0 1
# >> 	-> 1 1
# >> <- 1 1
# >> 	-> 0 2
# >> <- 0 2
# >> 	-> 1 2
# >> <- 1 2
# >> 	-> 0 3
# >> <- 0 3
# >> 	-> 1 3
# >> <- 1 3
# >> 	-> 0 4
# >> <- 0 4
# >> 	-> 1 4
# >> <- 1 4
# >> []

import asyncio

# worker x 2
# consumer x 1


async def worker(q, d, k):
    for i in range(5):
        print("\t>>", k, i)
        await q.put((k, i))
        await asyncio.sleep(d)


async def consumer(q):
    while True:
        k, i = await q.get()
        print("<<", k, i)
        q.task_done()


async def run():
    q = asyncio.Queue()
    loop = asyncio.get_event_loop()
    futs = [loop.create_task(worker(q, 0.3 + (0.1 * i), i)) for i in range(2)]
    ct = loop.create_task(consumer(q))
    await asyncio.wait(futs)
    await q.join()
    assert q.empty()
    ct.cancel()


asyncio.run(run(), debug=True)

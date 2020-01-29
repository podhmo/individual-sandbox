import asyncio


async def run():
    async def consume(q: asyncio.Queue):
        while True:
            item = await q.get()
            if item is None:
                q.task_done()
                break
            print(item)
            q.task_done()

    async def produce(q: asyncio.Queue, *, ev: asyncio.Event):
        for i in range(5):
            await q.put(i)
            ev.set()
        await q.put(None)

    q = asyncio.Queue()
    ev = asyncio.Event()

    futs = []
    futs.append(asyncio.ensure_future(consume(q)))
    futs.append(asyncio.ensure_future(produce(q, ev=ev)))

    def done(fut):
        print("ok")

    for fut in futs:
        fut.add_done_callback(done)

    await ev.wait()
    await q.join()


asyncio.run(run(), debug=True)

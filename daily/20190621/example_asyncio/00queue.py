import asyncio


async def producer(q, *, uid):
    for i in range(10):
        await q.put((uid, i))
        await asyncio.sleep(0.2)


async def consumer(q):
    while True:
        (uid, i) = await q.get()
        print(f"{uid}: {i}")
        q.task_done()


async def run():
    q = asyncio.Queue()
    loop = asyncio.get_event_loop()
    consumer_task = loop.create_task(consumer(q))
    await asyncio.wait([loop.create_task(producer(q, uid=uid)) for uid in range(2)])

    print("end")
    await q.join()
    assert q.empty()
    consumer_task.cancel()
    try:
        await consumer_task
    except asyncio.CancelledError:
        pass
    print("end..")


asyncio.run(run(), debug=True)
# cancel
# subprocessからの入力

import asyncio


async def run():
    async def peek(q: asyncio.Queue):
        while True:
            item = await q.get()
            if item is None:
                q.task_done()
                break
            print(item)
            q.task_done()

    q = asyncio.Queue()
    fut = asyncio.ensure_future(peek(q))

    def done(fut):
        print("ok")

    fut.add_done_callback(done)

    for i in range(5):
        q.put_nowait(i)
    q.put_nowait(None)
    await q.join()


asyncio.run(run(), debug=True)

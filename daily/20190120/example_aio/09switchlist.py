import asyncio

cur = []


async def reader(N):
    global cur
    i = 0
    for i in range(N):
        cur.append(i)
        i += 1
        await asyncio.sleep(0.1)


async def cutter(q):
    global cur
    while True:
        await asyncio.sleep(1)
        await q.put(cur)
        cur = []


async def writer(q):
    while True:
        v = await q.get()
        print("@", v)
        q.task_done()


async def main(loop):
    q = asyncio.Queue()
    r = reader(100)
    loop.create_task(cutter(q))
    loop.create_task(writer(q))
    await r
    await q.join()
    print("ok")


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop=loop))

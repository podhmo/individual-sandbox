import random
import asyncio


async def run():
    q = asyncio.Queue()

    async def do(afn, *args, **kwargs):
        await q.put((afn, args, kwargs))
        afn, args, kwargs = await q.get()
        try:
            return await afn(*args, **kwargs, add=q.put)
        finally:
            q.task_done()

    async def task(item, i, *, add=None):
        d, n = random.random(), random.random()
        print(f"{item:02}:{i}	d={d:.5f}	n={n:.5f}")
        await asyncio.sleep(d)

        if add is not None:
            if n > 0.75:
                await add((item, i + 1))

    for item in range(20):
        asyncio.create_task((task(item, 0)))

    await q.join()


asyncio.run(run())
print("ok")

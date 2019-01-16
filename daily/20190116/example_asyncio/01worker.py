import random
import asyncio


async def run():
    q = asyncio.Queue()

    async def worker():
        while True:
            item, i = await q.get()
            try:
                d, n = random.random(), random.random()
                print(f"{item:02}:{i}	d={d:.5f}	n={n:.5f}")
                await asyncio.sleep(d)
            finally:
                q.task_done()
            if n > 0.75:
                await q.put((item, i + 1))

    workers = [asyncio.create_task(worker()) for i in range(5)]

    for item in range(20):
        q.put_nowait((item, 0))

    await q.join()
    for w in workers:
        w.cancel()


asyncio.run(run())
print("ok")

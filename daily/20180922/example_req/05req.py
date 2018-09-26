import time


def req(i, level):
    print(i, level, "start")
    time.sleep(0.5)
    print(i, level, "end")
    return i, level + 1


import logging
import asyncio


async def run(loop):
    q = asyncio.Queue()
    r = []
    for i in range(5):
        q.put_nowait((i, 0))

    async def consume():
        while not q.empty():
            i, level = await q.get()
            x, next_level = await loop.run_in_executor(None, req, i, level)
            r.append((x, next_level))
            if next_level < 3:
                for j in range(x):
                    q.put_nowait((j, next_level))

    await asyncio.wait([consume() for _ in range(5)])
    return r


logging.basicConfig(level=logging.DEBUG)
st = time.time()
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))
print(time.time() - st)

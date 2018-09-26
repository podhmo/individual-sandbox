import time


def req(i, level):
    print(i, level, "start")
    time.sleep(0.5)
    print(i, level, "end")
    return i, level + 1


import logging
import asyncio


async def worker(loop, inq, outq):
    while True:
        val = await inq.get()
        if val is None:
            inq.task_done()
            break
        i, level = val
        result = await loop.run_in_executor(None, req, i, level)
        inq.task_done()
        await outq.put(result)


async def run(loop):
    inq = asyncio.Queue(maxsize=3)
    outq = asyncio.Queue()
    n = 7
    r = []

    # first step
    for i in range(5):
        outq.put_nowait((i, 0))

    for i in range(n):
        loop.create_task(worker(loop, outq, inq))

    # second step
    while True:
        if inq.empty():
            if outq.empty() and outq._unfinished_tasks == 0:
                for i in range(n):
                    outq.put_nowait(None)
                break
            else:
                await asyncio.sleep(0.5)
                continue
        i, level = await inq.get()
        r.append((i, level))
        if level < 3:
            for j in range(i):
                await outq.put((j, level))
        inq.task_done()
    return r


logging.basicConfig(level=logging.DEBUG)
st = time.time()
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))
print(time.time() - st)

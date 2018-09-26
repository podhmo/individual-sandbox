import time


def req(i, level):
    print(i, level, "start")
    time.sleep(0.5)
    print(i, level, "end")
    return i, level + 1


import logging
import asyncio


async def management(loop, inq, outq):
    r = []
    # second step
    while True:
        val = await inq.get()
        if val is None:
            inq.task_done()
            break
        i, level = val
        r.append((i, level))
        if level < 3:
            for j in range(i):
                await outq.put((j, level))
        inq.task_done()
    return r


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


async def check(loop, inq, outq, n):
    while True:
        if inq.empty() and outq.empty():
            if inq._unfinished_tasks == 0 and outq._unfinished_tasks == 0:
                await inq.put(None)
                for i in range(n):
                    await outq.put(None)
                break
        await asyncio.sleep(0.5)


async def run(loop):
    inq = asyncio.Queue(maxsize=10)
    outq = asyncio.Queue()
    futs = []

    # first step
    for i in range(5):
        outq.put_nowait((i, 0))

    futs.append(management(loop, inq, outq))
    for i in range(4):
        futs.append(worker(loop, outq, inq))
    futs.append(check(loop, inq, outq, 4))
    return await asyncio.wait(futs)


logging.basicConfig(level=logging.DEBUG)
st = time.time()
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))
print(time.time() - st)

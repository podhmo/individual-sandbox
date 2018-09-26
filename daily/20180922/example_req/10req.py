import time
import logging
import asyncio


def req(i):
    print(i, "start")
    time.sleep(0.5)
    print(i, "end")
    return i


async def req_async(i):
    print(i, "start")
    await asyncio.sleep(0.5)
    print(i, "end")
    return i


async def run_task_contextually(loop, fn, *args):
    if asyncio.iscoroutinefunction(fn):
        return await fn(*args)
    else:
        return await loop.run_in_executor(None, fn, *args)


async def run(loop):
    sem = asyncio.Semaphore(5)

    async def run_req(fn, i):
        async with sem:
            return await run_task_contextually(loop, fn, i)

    tasks = []
    tasks.extend([run_req(req, i) for i in range(3)])
    tasks.extend([run_req(req_async, i) for i in range(3, 6)])
    tasks.extend([run_req(req, i) for i in range(6, 9)])
    return await asyncio.gather(*tasks)


logging.basicConfig(level=logging.DEBUG)
st = time.time()
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))
print(time.time() - st)

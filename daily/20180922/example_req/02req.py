import time


def req(i):
    print(i, "start")
    time.sleep(0.5)
    print(i, "end")
    return i


import logging
import asyncio


async def run(loop):
    sem = asyncio.Semaphore(5)

    async def run_req(i):
        async with sem:
            return await loop.run_in_executor(None, req, i)

    tasks = [run_req(i) for i in range(20)]
    return await asyncio.gather(*tasks)


logging.basicConfig(level=logging.DEBUG)
st = time.time()
loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))
print(time.time() - st)

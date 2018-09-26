import time


def req(i):
    print(i, "start")
    time.sleep(0.5)
    print(i, "end")
    return i


import logging
import asyncio


async def run():
    async def run_req(i):
        return req(i)

    tasks = [run_req(i) for i in range(3)]
    return await asyncio.gather(*tasks)


logging.basicConfig(level=logging.DEBUG)
st = time.time()
print(asyncio.get_event_loop().run_until_complete(run()))
print(time.time() - st)

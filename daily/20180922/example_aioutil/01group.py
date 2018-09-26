import asyncio
import aioutil
import time


def req(i):
    print(i, "start")
    time.sleep(0.5)
    print(i, "end")
    return i


async def req_await(i):
    print(i, "start")
    await asyncio.sleep(0.5)
    print(i, "end")
    return i


st = time.time()
g = aioutil.Group(limit=5)
for i in range(10):
    g.go(req, i)
    g.go(req_await, i)
print(g.wait())
print(time.time() - st)

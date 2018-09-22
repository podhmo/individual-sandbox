import time
from functools import partial
import asyncio


def req(i):
    print(f"{i}: start")
    time.sleep(0.5)
    print(f"{i}: end")
    return i


def heavy_req(i):
    print(f"{i}: Hstart")
    time.sleep(2)
    print(f"{i}: Hend")
    return i


def run(loop):
    fns = [
        partial(req, 0),
        partial(heavy_req, 1),
        partial(req, 2),
        partial(req, 3),
        partial(req, 4),
        partial(req, 5),
    ]
    sem = asyncio.Semaphore(4)

    async def do_async(fn, *args, **kwargs):
        async with sem:
            return await loop.run_in_executor(None, fn, *args, **kwargs)

    return asyncio.gather(*[do_async(fn) for fn in fns], loop=loop)


loop = asyncio.get_event_loop()
print(loop.run_until_complete(run(loop)))

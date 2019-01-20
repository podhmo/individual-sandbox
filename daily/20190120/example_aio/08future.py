import asyncio


async def run():
    print(await f() + await g())


def f():
    fut = asyncio.Future()
    fut.set_result(10)
    return fut


async def g():
    await asyncio.sleep(0.1)
    return 10


asyncio.get_event_loop().run_until_complete(run())

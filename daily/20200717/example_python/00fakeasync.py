import asyncio


def hello_sync():
    print("hello sync")


async def hello_async():
    print("hello async")


def hello_future():
    loop = asyncio.get_event_loop()
    return asyncio.ensure_future(hello_async(), loop=loop)


def run(fn):
    ok = asyncio.iscoroutinefunction(fn)
    return (ok, fn, asyncio.iscoroutine(fn()))


print(run(hello_sync))
print(run(hello_async))
print(run(hello_future))
# `@asyncio.coroutine` はオワコン

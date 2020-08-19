import asyncio
import contextvars

IO = contextvars.ContextVar("IO")


async def do_captured(uid: int):
    from io import StringIO

    o = StringIO()
    IO.set(o)
    await do_task(uid)
    print(uid, o.getvalue())


async def do_task(uid: int):
    for i in range(10):
        await _do_task(uid, i)


async def _do_task(uid: int, i: int):
    o = IO.get()
    print(f"{uid}: {i}", file=o)
    await asyncio.sleep(0.1)


async def run():
    await asyncio.wait([do_captured(i) for i in range(3)])


asyncio.run(run(), debug=True)

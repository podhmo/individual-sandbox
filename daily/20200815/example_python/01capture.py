import asyncio
from handofcats import as_command


async def do_captured(uid: int):
    from io import StringIO
    from contextlib import redirect_stdout

    o = StringIO()
    with redirect_stdout(o):
        await do_task(uid)
    print(uid, o.getvalue())


async def do_task(uid: int):
    for i in range(10):
        print(f"{uid}: {i}")
        await asyncio.sleep(0.1)


@as_command
def run():
    async def run():
        await asyncio.wait([do_captured(i) for i in range(3)])

    asyncio.run(run(), debug=True)

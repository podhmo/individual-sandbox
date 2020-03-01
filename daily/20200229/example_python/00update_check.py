import sys
import typing as t
import asyncio
import random
from handofcats import as_command, print




async def update_check() -> t.Awaitable[t.Optional[str]]:
    print("-> update_check ...")
    await asyncio.sleep(0.5)
    print("<- ... update_check")

    if random.random() < 0.2:
        # update is not found
        return None

    # update is found
    return "0.8.8"


@as_command
def main():
    async def run():
        fut = asyncio.ensure_future(update_check())

        print("do something (main)")
        for i in range(6):
            print(".")
            sys.stdout.flush()
            await asyncio.sleep(0.1)
        print("ok")

        update_version = await fut
        if update_version is not None:
            print(f"A new release of gh is available: xxx → {update_version}")

    asyncio.run(run())

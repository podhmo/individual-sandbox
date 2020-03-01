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


def run_main():
    import time

    print("do something (main)")
    for i in range(6):
        print(".")
        time.sleep(0.1)
    print("ok")


@as_command
def main():
    async def run():
        fut = asyncio.ensure_future(update_check())
        run_main()
        update_version = await fut
        if update_version is not None:
            print(f"A new release of gh is available: xxx → {update_version}")

    asyncio.run(run())

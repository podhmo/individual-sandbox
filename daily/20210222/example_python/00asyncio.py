import asyncio
from handofcats import as_command


async def do_task(prefix):
    for i in range(3):
        print(prefix, i)
        await asyncio.sleep(0.5)
        print(prefix, i, "end")


@as_command
def run():
    async def main():
        await asyncio.wait(
            [do_task("x"), do_task("y"), do_task("z"),]
        )

    asyncio.run(main())

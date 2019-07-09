import asyncio
import sys


async def get_date():
    code = "import time; [(print(i) or time.sleep(0.3)) for i in range(10)]"

    proc = await asyncio.create_subprocess_exec(
        sys.executable, "-u", "-c", code, stdout=asyncio.subprocess.PIPE
    )

    loop = asyncio.get_event_loop()

    async def consume():
        async for data in proc.stdout:
            print("@", data)

    t = loop.create_task(consume())

    # Wait for the subprocess exit.
    await proc.wait()
    await t


if sys.platform == "win32":
    asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())

asyncio.run(get_date())

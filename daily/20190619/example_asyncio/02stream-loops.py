import asyncio
from asyncio.queues import Queue
import sys


async def get_data(q, i):
    code = "import time; [(print(i) or time.sleep(0.3)) for i in range(10)]"

    proc = await asyncio.create_subprocess_exec(
        sys.executable, "-u", "-c", code, stdout=asyncio.subprocess.PIPE
    )

    loop = asyncio.get_event_loop()

    async def consume(i):
        async for data in proc.stdout:
            await q.put((i, data.decode("utf-8")))

    t = loop.create_task(consume(i))

    # Wait for the subprocess exit.
    await proc.wait()
    await t


async def main():
    q = Queue()
    loop = asyncio.get_event_loop()
    running = True
    ts = [get_data(q, i) for i in range(3)]

    async def consume():
        nonlocal running
        while running:
            try:
                print(await asyncio.wait_for(q.get(), timeout=1))
                q.task_done()
            except asyncio.TimeoutError as e:
                print("hmm", e)
        print("ok")
        while not q.empty():
            print(await q.get())
        print("ok..")

    ct = asyncio.get_event_loop().create_task(consume())
    await asyncio.wait(ts)
    running = False
    print("end")
    # ct.cancel()  # xxx
    await ct
    print("end..")
    assert q.empty()
    await q.join()
    print("end...")


asyncio.run(main())

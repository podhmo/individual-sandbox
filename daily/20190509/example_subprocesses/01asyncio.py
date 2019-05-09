import logging
from functools import partial
import signal
import asyncio


async def run():
    running = True
    loop = asyncio.get_event_loop()

    p = await asyncio.create_subprocess_exec(
        "python",
        "./gen.py",
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        limit=4096,
    )

    def handler(*, signum):
        nonlocal running
        running = False
        p.send_signal(signum)
        print("yay", signum)

    async def consume(*, out, base=35, i=0):
        nonlocal running
        while running:
            line = await out.readline()
            if not line:
                break
            print(f"\x1b[{base+i}m{line.decode('utf-8')}\x1b[0m", end="")
            # print("@", line.decode("utf-8").rstrip())

    # unix only
    loop.add_signal_handler(signal.SIGINT, partial(handler, signum=signal.SIGINT))

    # Wait for the subprocess exit.
    await asyncio.wait(
        [consume(out=p.stdout), consume(out=p.stderr, i=1), p.wait()],
        # return_when=asyncio.FIRST_COMPLETED,
    )


logging.basicConfig(level=logging.DEBUG)
print(asyncio.run(run(), debug=False))

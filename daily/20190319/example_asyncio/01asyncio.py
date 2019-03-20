import time
import signal
import asyncio


async def run():
    loop = asyncio.get_event_loop()

    def callback(*args, **kwargs):
        print("@async", args, kwargs)

    loop.add_signal_handler(signal.SIGINT, callback)
    for i in range(5):
        print("hello", i)
        await asyncio.sleep(1)


print("async")
import logging

logging.basicConfig(level=logging.DEBUG)
loop = asyncio.get_event_loop()
# asyncio.run(run(), debug=True)
loop.run_until_complete(run())
print("sync")


def handler(signum, tb):
    print("@sync", signum, tb)


signal.signal(signal.SIGINT, handler)
for i in range(10):
    print("hello", i)
    time.sleep(1)

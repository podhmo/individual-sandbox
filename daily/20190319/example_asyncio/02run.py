import logging
import signal
import asyncio


async def task():
    await asyncio.sleep(1)
    return "done"


async def timeout():
    await asyncio.sleep(100)
    return "cancel"


async def run(task, timeout=2):
    loop = asyncio.get_running_loop()
    result = None
    ev = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev.set)
    done, pending = await asyncio.wait(
        [task(), ev.wait()], timeout=timeout, return_when=asyncio.FIRST_COMPLETED
    )
    if done:
        result = "done"
    else:
        result = "timeout"
    print(done, pending)
    print(f"@@result {result} is_cancel={ev.is_set()}")


logging.basicConfig(level=logging.DEBUG)
debug = False
print("--ok------------------------------------")
asyncio.run(run(task), debug=debug)

print("--timeout-------------------------------")
asyncio.run(run(timeout, timeout=0), debug=debug)

print("--cancel--------------------------------")
asyncio.run(run(timeout, timeout=100), debug=debug)

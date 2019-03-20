import asyncio
import halo


def spinner(itr, *, text=None):
    s = halo.Halo(text=text)
    s.start()
    try:
        for i, x in enumerate(itr):
            # s.info(f"."*i)
            s.stop_and_persist(text="." * (i+1))
            s.start()
            yield x
    finally:
        s.succeed("OK")


async def do_task(text):
    for _ in spinner(range(4), text=text):
        await asyncio.sleep(0.3)


async def run():
    await do_task("loadingA")
    await do_task("loadingB")


# asyncio.run(run(), debug=True)
asyncio.run(run(), debug=False)

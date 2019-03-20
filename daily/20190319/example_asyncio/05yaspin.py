import asyncio
import logging


async def run():
    await asyncio.wait([task2(desc="A")])


async def task2(*, desc=None):
    from yaspin import yaspin

    with yaspin(text="Colors!") as sp:
        # Support all basic termcolor text colors
        colors = ("red", "green", "yellow", "blue", "magenta", "cyan", "white")

        for color in colors:
            sp.color, sp.text = color, color
            await asyncio.sleep(1)


logging.basicConfig(level=logging.DEBUG)
asyncio.run(run(), debug=True)

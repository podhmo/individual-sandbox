import typing as t
import sys


class Handler:
    def __init__(self, *, prompt, input):
        self.prompt = prompt
        self.input = input

    async def run(self):
        import re

        prompt = self.prompt.prompt
        get_input = self.input.get_input

        await prompt("open <https://example.net> in browser and ...")
        rx = re.compile(r"[0-9a-f]{4}")
        await prompt(f"  (expected {rx})")
        life = 3

        result = None
        for _ in range(life):
            text = await get_input()
            if rx.search(text) is not None:
                result = text
                break
            await prompt("invalid text")

        if result is not None:
            await prompt(f"ok: {result}")
        else:
            await prompt("giveup")


class ConsolePrompt:
    def __init__(self, *, writer: t.Optional[t.IO[str]] = None) -> None:
        self.writer = writer or sys.stderr

    async def prompt(self, text: str) -> None:
        print(text, file=self.writer)


class ConsoleInput:
    def __init__(self, *, reader: t.Optional[t.IO[str]] = None) -> None:
        self.reader = reader or sys.stdin

    async def get_input(self) -> str:
        return self.reader.readline().strip()


import asyncio

asyncio.run(Handler(prompt=ConsolePrompt(), input=ConsoleInput()).run(), debug=True)

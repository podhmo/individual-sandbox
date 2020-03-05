import typing as t
import logging
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


mapping = {
    "TRACE": "[ trace ]",
    "DEBUG": "[ \x1b[0;36mdebug\x1b[0m ]",
    "INFO": "[  \x1b[0;32minfo\x1b[0m ]",
    "WARNING": "[  \x1b[0;33mwarn\x1b[0m ]",
    "WARN": "[  \x1b[0;33mwarn\x1b[0m ]",
    "ERROR": "\x1b[0;31m[ error ]",
    "ALERT": "\x1b[0;37;41m[ alert ]",
    "CRITICAL": "\x1b[0;37;41m[ alert ]",  # alert level is not found in python
}


class ColorfulHandler(logging.StreamHandler):
    def emit(self, record: logging.LogRecord) -> None:
        record.levelname = mapping[record.levelname]
        super().emit(record)


@as_command
def main():
    for h in logging.root.handlers:
        if isinstance(h, logging.StreamHandler):
            h.__class__ = ColorfulHandler

    async def run():
        fut = asyncio.ensure_future(update_check())
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, run_main)
        update_version = await fut
        if update_version is not None:
            print(f"A new release of gh is available: xxx → {update_version}")

    asyncio.run(run())

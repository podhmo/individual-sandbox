# https://bugs.python.org/issue23548
import asyncio


@asyncio.coroutine
def main():
    p = yield from asyncio.create_subprocess_shell('echo hi')
    yield from p.wait()

loop = asyncio.get_event_loop()
loop.run_until_complete(main())
loop.close()

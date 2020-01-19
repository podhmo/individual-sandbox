import typing as t
import itertools
import subprocess
import sys
import handofcats
import asyncio


@handofcats.as_subcommand
def main():
    p = subprocess.Popen(
        [sys.executable, "-u", __file__, "sub"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False,  # for read1
    )
    loop = asyncio.get_event_loop()
    assert isinstance(loop, asyncio.SelectorEventLoop)

    q = asyncio.Queue()

    def callback(port: t.IO[bytes], stdout: bool = True):
        data = port.read1().decode()
        if data == "":
            loop.remove_reader(port)
            q.task_done()
            return

        if stdout:
            print("STDOUT", data, end="")
        else:
            print("STDERR", data, end="", file=sys.stderr)

    # todo: signal handling

    q.put_nowait(None)
    q.put_nowait(None)
    loop.add_reader(p.stdout.fileno(), callback, p.stdout, True)
    loop.add_reader(p.stderr.fileno(), callback, p.stderr, False)

    async def run():
        await q.join()
        print("OK")

    loop.run_until_complete(run())


@handofcats.as_subcommand
def sub():
    import sys
    from time import sleep

    for i in range(10):
        print(f" x{i} ", file=sys.stderr)
        sleep(0.1)
        print(f" y{i} ")
        sleep(0.1)


handofcats.as_subcommand.run()

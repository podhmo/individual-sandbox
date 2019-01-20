import sys
import asyncio
import subprocess
from io import BytesIO


async def run(loop, *, second: int, command: str):
    buf = [BytesIO()]
    q = asyncio.Queue()
    tmpq = asyncio.Queue()

    async def reader():
        def _run():
            for line in iter(sys.stdin.readline, None):
                if not line:
                    break
                buf[0].write(line.encode("utf-8"))

        def finish(fut):
            q.put_nowait(buf[0])
            q.put_nowait(None)

        fut = loop.run_in_executor(None, _run)
        fut.add_done_callback(finish)
        return await fut

    async def cutter():
        while True:
            await asyncio.sleep(1, loop=loop)
            tmp = buf[0]
            buf[0] = BytesIO()
            await q.put(tmp)

    async def communicator():
        i = 0

        async def communicate(v):
            nonlocal i
            n = i
            i += 1
            p = await asyncio.create_subprocess_shell(
                command,
                loop=loop,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
            )
            stdout, _ = await p.communicate(v)
            await tmpq.put((n, stdout))

        while True:
            o = await q.get()
            if o is None:
                q.task_done()
                break
            fut = loop.create_task(communicate(o.getvalue()))
            fut.add_done_callback(lambda fut: q.task_done())

    async def writer():
        i = 0
        pending = {}
        while True:
            o = await tmpq.get()
            if o is None:
                tmpq.task_done()
                break
            j, output = o
            pending[j] = output
            if i != j:
                continue
            while i in pending:
                print(pending.pop(i).decode("utf-8").strip("\n"))
                i += 1
                tmpq.task_done()
            sys.stdout.flush()

    futs = []
    futs.append(loop.create_task(cutter()))
    futs.append(loop.create_task(communicator()))
    futs.append(loop.create_task(writer()))
    await reader()
    await q.join()
    await tmpq.join()
    for fut in futs:
        fut.cancel()


def main():
    import logging
    logging.basicConfig(level=logging.DEBUG)

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--second", type=int)
    parser.add_argument("-c", "--command")
    args = parser.parse_args()

    loop = asyncio.get_event_loop()
    loop.run_until_complete(run(loop, **vars(args)))


if __name__ == "__main__":
    main()

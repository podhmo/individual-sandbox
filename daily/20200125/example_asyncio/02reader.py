import asyncio
import logging
from handofcats import as_subcommand

logger = logging.getLogger(__name__)
WORKER_FILE = "./02.worker.fifo"


@as_subcommand
def run():
    import sys
    import subprocess

    wp = subprocess.Popen([sys.executable, __file__, "writer"])
    # rp = subprocess.Popen([sys.executable, __file__, "reader"])
    rp = subprocess.Popen([sys.executable, __file__, "aio_reader"])

    wp.wait()
    rp.wait()


@as_subcommand
def reader():
    import time

    time.sleep(0.1)
    with open(WORKER_FILE, "r") as rf:
        for line in rf:
            logger.info("recv: %r", line)
            print("got", line)


@as_subcommand
def aio_reader():
    async def _run():
        q = asyncio.Queue()

        async def consume(*, uid: int):
            while True:
                item = await q.get()
                if item is None:
                    q.task_done()
                    break
                await asyncio.sleep(0.2)
                print("got", repr(item), uid)
                q.task_done()

        # worker
        for uid in range(4):
            asyncio.ensure_future(consume(uid=uid))

        await asyncio.sleep(0.1)
        with open(WORKER_FILE, "r") as rf:
            for line in iter(rf.readline, ""):
                logger.info("recv: %r", line)
                await q.put(line)
                await asyncio.sleep(0.01)  # hmm?
            await q.put(None)
        await q.join()
        print("OK")

    asyncio.run(_run(), debug=True)


@as_subcommand
def writer():
    import time
    import pathlib
    import os

    path = pathlib.Path(WORKER_FILE)
    if path.exists():
        path.unlink()
    os.mkfifo(path)

    def _opener(name: str, flags: int) -> int:
        return os.open(name, os.O_WRONLY)

    try:
        with open(path, "w", opener=_opener) as wf:
            for i in range(10):
                msg = f"hello: {i}"
                logger.info("send: %r", msg)
                print(msg, file=wf)
                wf.flush()
                time.sleep(0.1)
    finally:
        path.unlink()


as_subcommand.run()

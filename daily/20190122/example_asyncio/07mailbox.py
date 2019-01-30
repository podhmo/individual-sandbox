import asyncio
import logging
import os
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
logger = logging.getLogger(__name__)


class Queue(asyncio.Queue):
    def put_nowait(self, item):
        logger.debug("put: %s", item)
        return super().put_nowait(item)

    def get_nowait(self):
        item = super().get_nowait()
        logger.debug("get: %s", item)
        return item


async def writer(ctx):
    loop = ctx["loop"]
    q = ctx["writer"]

    while True:
        v = await q.get()
        if v is None:
            q.task_done()
            break
        print("w", v)
        q.task_done()
    print("close", "w")
    await q.join()


async def generator(ctx, n):
    loop = ctx["loop"]
    q = ctx["communicator"]

    for i in range(n):
        await q.put(i)
        await asyncio.sleep(0.1, loop=loop)
    print("close", "g")
    await q.put(None)


async def communicator(ctx):
    inq = ctx["communicator"]
    outq = ctx["writer"]

    while True:
        v = await inq.get()
        if v is None:
            inq.task_done()
            break
        await outq.put(v * v)
        inq.task_done()
    print("close", "c")
    await outq.put(None)
    await inq.join()


async def watcher(ctx, queues):
    loop = ctx["loop"]
    while True:
        for i, q in enumerate(queues):
            print(i, "**", q.qsize(), q._unfinished_tasks)
        await asyncio.sleep(1, loop=loop)


async def main(loop):
    queues = []
    tasks = []

    wq = Queue()
    cq = Queue()
    ctx = {
        "loop": loop,
        "communicator": cq,
        "writer": wq,
    }
    queues.append(cq)
    queues.append(wq)

    tasks.append(loop.create_task(writer(ctx)))
    tasks.append(loop.create_task(communicator(ctx)))
    tasks.append(loop.create_task(generator(ctx, 10)))
    w = loop.create_task(watcher(ctx, queues))
    await asyncio.wait(tasks, loop=loop)
    print("ok")
    for q in queues:
        await q.join()
    w.cancel()


loop = asyncio.get_event_loop()
loop.run_until_complete(main(loop))
loop.close()

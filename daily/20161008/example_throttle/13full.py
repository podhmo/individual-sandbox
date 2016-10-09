import time
import asyncio
import logging
import lib
logger = logging.getLogger(__name__)


async def do_loop(requests):
    semaphore = asyncio.Semaphore(10)

    todo = asyncio.Queue()
    done = asyncio.Queue()

    async def to_future(request):
        async with semaphore:
            return await lib.mock_fetch(request)

    async def done_worker():
        while True:
            request = await done.get()
            fut = asyncio.ensure_future(to_future(request))
            todo.put_nowait(fut)

    async def todo_worker():
        def callback(fut):
            todo.task_done()
            response = fut.result()
            for request in response.get_links():
                done.put_nowait(request)
        while True:
            fut = await todo.get()
            fut.add_done_callback(callback)

    workers = []
    workers.append(asyncio.ensure_future(done_worker()))
    workers.append(asyncio.ensure_future(todo_worker()))

    st = time.time()
    for req in requests:
        done.put_nowait(req)

    while not (todo._unfinished_tasks == 0 and len(done._queue) == 0):
        print("loop")
        logger.info("loop: todo=%s, done=%s", todo._unfinished_tasks, len(done._queue))
        await asyncio.sleep(0.2)

    for w in workers:
        w.cancel()
    logger.info("takes %s, total %s", time.time() - st, done._unfinished_tasks)

if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # from mock_long_requests import requests
    from mock_long_requests import requests
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()


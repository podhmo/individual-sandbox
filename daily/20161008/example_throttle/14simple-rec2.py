import time
import asyncio
import logging
import lib
logger = logging.getLogger(__name__)


async def do_loop(requests):
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)
    semaphore = asyncio.Semaphore(10)
    q = asyncio.Queue()

    async def fetch_urls(request):
        async with semaphore:
            response = await dispatcher.dispatch(lib.mock_fetch, request)
            return response.get_links()

    async def do_work():
        while True:
            item = await q.get()
            links = await fetch_urls(item)
            q.task_done()
            for link in links:
                await q.put(link)

    workers = []
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))
    workers.append(asyncio.ensure_future(do_work()))

    st = time.time()
    for req in requests:
        await q.put(req)

    while not (q.empty() and q._unfinished_tasks <= 0):  # xxx
        logger.info("loop: unfinished=%s, queue=%s", q._unfinished_tasks, len(q._queue))
        await asyncio.sleep(0.2)
    for w in workers:
        w.cancel()
    logger.info("takes %s, total %s", time.time() - st, "x")


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # from mock_long_requests import requests
    from mock_long_requests import requests
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()

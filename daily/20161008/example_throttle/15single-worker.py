import time
import asyncio
import logging
import lib
from collections import defaultdict
logger = logging.getLogger(__name__)


class PENDING:
    def __init__(self, request):
        self.request = request


async def do_loop(requests):
    history = {}  # domain -> time
    count = defaultdict(int)  # domain -> int
    q = asyncio.Queue()
    asyncq = asyncio.Queue()
    delta = 1
    total = 0
    max_request_for_domain = 2
    concurrency = 4
    semaphore = asyncio.Semaphore(concurrency)

    async def pending(p):
        request = p.request
        logger.info("pending: %s %s", request.domain, request.args)
        await asyncio.sleep(1.0)
        return request

    async def fetch_urls(request):
        nonlocal total
        t0 = history.get(request.domain)
        t = time.time()
        history[request.domain] = t
        c = count[request.domain]
        # logger.info("t=%s, t0=%s,  t-t0=%s", t, t0, t0 and t - t0)
        if c > max_request_for_domain and t0 and (t - t0) < delta:
            return [PENDING(request)]

        async with semaphore:
            count[request.domain] += 1
            response = await lib.mock_fetch(request)
            count[request.domain] -= 1
            total += 1
            return response.get_links()

    async def do_work():
        while True:
            item = await q.get()
            links = await fetch_urls(item)
            q.task_done()
            for link in links:
                if isinstance(link, PENDING):
                    asyncq.put_nowait(asyncio.ensure_future(pending(link)))
                else:
                    await q.put(link)

    async def do_async_work():
        while True:
            fut = await asyncq.get()
            await fut
            request = fut.result()
            asyncq.task_done()
            logger.info("insert from asyncq: %s %s", request.domain, request.args)
            q.put_nowait(request)

    workers = []
    workers.append(asyncio.ensure_future(do_async_work()))
    for i in range(concurrency):
        workers.append(asyncio.ensure_future(do_work()))

    st = time.time()
    for req in requests:
        await q.put(req)

    while not (q.empty() and q._unfinished_tasks <= 0 and asyncq.empty() and asyncq._unfinished_tasks <= 0):  # xxx
        logger.info("loop: unfinished=%s, queue=%s", q._unfinished_tasks, len(q._queue))
        logger.info("loop: async unfinished=%s, queue=%s", asyncq._unfinished_tasks, len(asyncq._queue))
        await asyncio.sleep(0.2)
    for w in workers:
        w.cancel()
    logger.info("takes %s, total %s", time.time() - st, total)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # from mock_long_requests import requests
    from mock_long_requests import requests
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()

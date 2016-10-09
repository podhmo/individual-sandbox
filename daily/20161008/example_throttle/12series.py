import time
import asyncio
import logging
import lib
logger = logging.getLogger(__name__)

async def do_loop(requests):
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)

    todo = []
    done = []
    st = time.time()
    for r in requests:
        response = await dispatcher.dispatch(lib.mock_fetch, r)
        done.append(r)
        todo.extend(response.get_links())

    while todo:
        r = todo.pop()
        response = await dispatcher.dispatch(lib.mock_fetch, r)
        done.append(r)
        todo.extend(response.get_links())
    logger.info("takes %s, total %s", time.time() - st, len(done))

if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # from mock_long_requests import requests
    from mock_long_requests import requests
    # random.shuffle(requests)
    # 0.2 * 60 = 12.0
    loop.run_until_complete(do_loop(requests))
    loop.close()

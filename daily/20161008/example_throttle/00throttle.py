import random
import asyncio
import logging
logger = logging.getLogger(__name__)


async def to_throttle_tasks(xs):
    N = 2
    for i in range(0, len(xs), N):
        buf = []
        for j in range(N):
            if i + j < len(xs):
                buf.append(xs[i + j])
        buf.append(asyncio.sleep(1))
        await asyncio.gather(*buf)


async def tasknize(x):
    d = 2 * random.random()
    logger.info("before: %s cost=%s", x, d)
    await asyncio.sleep(d)
    logger.info("after: %s", x)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    xs = ["a", "b", "c", "d", "e"]
    loop.run_until_complete(to_throttle_tasks([tasknize(x) for x in xs]))

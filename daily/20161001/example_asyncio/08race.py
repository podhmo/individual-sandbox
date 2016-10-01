import asyncio
import logging
logger = logging.getLogger(__name__)

async def do_task(uid, n):
    logger.info("start task uid=%s, wait=%s", uid, n)
    await asyncio.sleep(n)
    logger.info("end task uid=%s", uid)
    return uid


async def do_loop():
    logger.info("start loop")
    cols = [do_task("a", 1), do_task("b", 0.5), do_task("c", 2)]
    results, pendings = await asyncio.wait(cols, return_when=asyncio.FIRST_COMPLETED)
    for fut in results:
        logger.info("result: uid=%s", fut.result())
    for fut in pendings:
        fut.cancel()
    logger.info("end loop")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s")
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
    loop.close()

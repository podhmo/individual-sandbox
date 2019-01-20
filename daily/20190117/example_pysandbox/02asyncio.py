import os
import logging
import asyncio
import subprocess
logger = logging.getLogger(__name__)

logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


async def main(n):
    logger.info("hello")
    await asyncio.sleep(n)
    p = await asyncio.create_subprocess_shell(
        "sleep 1 && ls",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    print((await p.communicate())[0].decode("utf-8"))
    logger.info("bye")


loop = asyncio.get_event_loop()
loop.run_until_complete(main(0.1))
loop.close()

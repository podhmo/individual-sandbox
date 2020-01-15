import asyncio
import websockets
import subprocess
from handofcats import as_command
import logging

logger = logging.getLogger(__name__)


def spawn(*, i: int):
    p = subprocess.Popen(
        ["python", "counter.py"], stdout=subprocess.PIPE, text=True, bufsize=1
    )
    for line in p.stdout:
        logger.info(line)
        yield line
    p.wait()


@as_command
def run():
    async def push(websocket, path):
        for line in spawn(i=0):
            await websocket.send(line)

    asyncio.get_event_loop().run_until_complete(
        websockets.serve(push, "localhost", 8765)
    )
    asyncio.get_event_loop().run_forever()

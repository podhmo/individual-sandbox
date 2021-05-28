import os
import sys
import logging
import asyncio
import websockets
from websockets.exceptions import ConnectionClosedError
from asyncio.exceptions import TimeoutError


async def hello(websocket, path):
    try:
        d = 1
        i = 0
        while True:
            try:
                n = await asyncio.wait_for(websocket.recv(), timeout=0.5)
                d = int(n)
            except TimeoutError:
                pass
            except TypeError as e:
                print("!!", e, file=sys.stderr)

            i += d
            await websocket.send(str(i))
    except ConnectionClosedError:
        pass


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)
start_server = websockets.serve(hello, "localhost", PORT)

loop = asyncio.get_event_loop()
loop.run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

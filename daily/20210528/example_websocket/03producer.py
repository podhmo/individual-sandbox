import os
import logging
import asyncio
import websockets
from websockets.exceptions import ConnectionClosedError

connected = set()


async def hello(websocket, path):
    try:
        i = 0
        while True:
            print(">", i)
            await websocket.send(str(i))
            await asyncio.sleep(0.5)
            i += 1
    except ConnectionClosedError:
        pass


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)
start_server = websockets.serve(hello, "localhost", PORT)

loop = asyncio.get_event_loop()
loop.run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

import os
import logging
import asyncio
import websockets
from websockets.exceptions import ConnectionClosedError


async def hello(websocket, path):
    try:
        async for name in websocket:
            name = name.strip()
            print("< {}".format(name))

            greeting = "Hello {}!".format(name)
            await websocket.send(greeting)
            print("> {}".format(greeting))
    except ConnectionClosedError as ex:
        print(type(ex), ex)


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)
start_server = websockets.serve(hello, "localhost", PORT)

loop = asyncio.get_event_loop()
loop.run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

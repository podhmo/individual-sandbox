import os
import logging
import asyncio
import websockets
from websockets.exceptions import ConnectionClosedError

connected = set()


async def hello(websocket, path):
    try:
        connected.add(websocket)
        async for name in websocket:
            name = name.strip()
            print("< {}".format(name))

            greeting = "Hello {}!".format(name)
            print("> {}".format(greeting))
            await asyncio.wait([w.send(greeting) for w in connected])

    except ConnectionClosedError as ex:
        print(type(ex), ex)
    finally:
        connected.remove(websocket)


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)
start_server = websockets.serve(hello, "localhost", PORT)

loop = asyncio.get_event_loop()
loop.run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

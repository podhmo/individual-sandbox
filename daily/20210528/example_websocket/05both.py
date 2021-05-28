import os
import sys
import logging
import asyncio
import websockets
from websockets.exceptions import ConnectionClosedError


async def hello(websocket, path):
    try:
        d = 1
        i = 0
        while True:
            recv_task = asyncio.ensure_future(websocket.recv())
            send_task = asyncio.ensure_future(websocket.send(str(i)))
            done, pending = await asyncio.wait(
                [recv_task, send_task], return_when=asyncio.FIRST_COMPLETED
            )
            for t in pending:
                t.cancel()

            if recv_task in done:
                try:
                    d = int(recv_task.result())
                except TypeError as e:
                    print("!!", e, file=sys.stderr)

            i += d
            await asyncio.sleep(0.5)
    except ConnectionClosedError:
        pass


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)
start_server = websockets.serve(hello, "localhost", PORT)

asyncio.run(start_server, debug=True)
asyncio.get_event_loop().run_forever()

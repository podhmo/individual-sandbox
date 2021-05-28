import logging
import os
import signal
import asyncio
import websockets


async def hello(websocket, path):
    try:
        while True:
            name = await websocket.recv()
            name = name.strip()
            print("< {}".format(name))

            greeting = "Hello {}!".format(name)
            await websocket.send(greeting)
            print("> {}".format(greeting))
    except websockets.ConnectionClosedError:
        print("########################################")


DEBUG = bool(int(os.getenv("DEBUG") or "0"))
PORT = int(os.getenv("PORT") or "8888")

logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO)


async def run():
    # 引数は使わないけれど、SIGINTに貼り付けたときなどに実行に失敗するといい感じに死ぬのでこれは危険？
    ev = asyncio.Event()
    asyncio.get_event_loop().add_signal_handler(signal.SIGINT, ev.set)

    async with websockets.serve(hello, "localhost", PORT) as server:
        print("start server", server)
        await ev.wait()


asyncio.run(run(), debug=DEBUG)

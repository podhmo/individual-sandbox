import os
import asyncio
import websockets

PORT = int(os.getenv("PORT") or "8888")


async def hello():
    async with websockets.connect(f"ws://localhost:{PORT}") as websocket:

        name = input("What's your name? ")
        await websocket.send(name)
        print("> {}".format(name))

        greeting = await websocket.recv()
        print("< {}".format(greeting))


asyncio.run(hello(), debug=True)

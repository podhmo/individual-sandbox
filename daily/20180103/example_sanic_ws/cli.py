import asyncio
import websockets


async def hello(uri):
    async with websockets.connect(uri) as websocket:
        print("send:", "Hello world!")
        await websocket.send("Hello world!")
        msg = await websocket.recv()
        print("recv:", msg)


asyncio.get_event_loop().run_until_complete(hello('ws://localhost:8000/feed'))

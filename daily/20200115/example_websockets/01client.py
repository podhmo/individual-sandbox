import asyncio
import websockets


async def hello(uri):
    print("start")
    async with websockets.connect(uri) as websocket:
        for i in range(10):
            print("send")
            await websocket.send("Hello world!")
            print("...")
            message = await websocket.recv()
            print("received", message)


asyncio.get_event_loop().run_until_complete(hello("ws://localhost:8765"))

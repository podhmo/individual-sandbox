import asyncio
import websockets


async def view(uri):
    print("start")
    async with websockets.connect(uri) as websocket:
        async for message in websocket:
            print("received", message, end="")


asyncio.get_event_loop().run_until_complete(view("ws://localhost:8765"))

import asyncio
import websockets


async def hello():
    async with websockets.connect('ws://localhost:8080/ws') as websocket:
        while True:
            data = input(">")
            await websocket.send(data)
            msg = await websocket.recv()
            print('Message received from server:', msg)


asyncio.get_event_loop().run_until_complete(hello())

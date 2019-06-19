import asyncio
import websockets


async def hello():
    async with websockets.connect("ws://127.0.0.1:8000/ws") as websocket:
        while True:
            name = input("What's your name? ")
            await websocket.send(name)
            print(f"> {name}")

            greeting = await websocket.recv()
            print(f"< {greeting}")


# asyncio.run(hello(), debug=True)
asyncio.run(hello())

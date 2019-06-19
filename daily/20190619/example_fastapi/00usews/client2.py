import asyncio
import websockets


async def hello():
    async with websockets.connect("ws://127.0.0.1:8000/ws") as websocket:

        async def consume():
            async for greeting in websocket:
                print(f"< {greeting}")
                1 / 0

        loop = asyncio.get_event_loop()
        t = loop.create_task(consume())

        while True:
            name = input("What's your name? ")
            await websocket.send(name)
            print(f"> {name}")
        await t  # await付けといたほうが良い？


# asyncio.run(hello(), debug=True)
asyncio.run(hello())

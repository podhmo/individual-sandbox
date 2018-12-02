import asyncio
import websockets


async def handle(websocket, path):
    while True:
        data = await websocket.recv()
        print(data)
        if data == "close":
            await websocket.close()
        else:
            await websocket.send(data)


start_server = websockets.serve(handle, '0.0.0.0', 8080)
asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()

import asyncio
import websockets


i = 0
async def echo(websocket, path):
    global i
    async for message in websocket:
        i += 1
        print(f"<- {i:02d}: {message}")
        await websocket.send(message)


asyncio.get_event_loop().run_until_complete(websockets.serve(echo, "localhost", 8765))
asyncio.get_event_loop().run_forever()

import sys
import subprocess
from handofcats import as_subcommand

# from https://github.com/aaugustin/websockets


@as_subcommand
def run():
    sp = subprocess.Popen([sys.executable, __file__, "server", "--port", "8765"])
    cp = subprocess.Popen([sys.executable, __file__, "client", "--port", "8765"])
    cp.wait()
    sp.terminate()


@as_subcommand
def server(*, port: int):
    import asyncio
    import websockets

    async def echo(websocket, path):
        async for message in websocket:
            await websocket.send(message)

    asyncio.get_event_loop().run_until_complete(
        websockets.serve(echo, "localhost", port)
    )
    asyncio.get_event_loop().run_forever()


@as_subcommand
def client(*, port: int):
    import asyncio
    import websockets

    async def hello(uri):
        async with websockets.connect(uri) as websocket:
            await websocket.send("Hello world!")
            await websocket.recv()

    endpoint = f"ws://localhost:{port}"
    asyncio.get_event_loop().run_until_complete(hello(endpoint))


as_subcommand.run()

import asyncio
import websockets
from handofcats import as_command


@as_command
def run(uri: str) -> None:
    async def app():
        async with websockets.connect(uri) as ws:
            print("<-", await ws.recv())

    asyncio.run(app(), debug=True)

import sys
import asyncio


class UpcaseServerProtocol(asyncio.Protocol):
    def connection_made(self, transport: asyncio.Transport):
        info = transport._extra  # xxx
        print(f"Connection info: {info}", file=sys.stderr)
        self.transport = transport

    def data_received(self, data):
        msg = data.decode()
        print(f"got: {msg}", file=sys.stderr)
        self.transport.write(msg.upper().encode("utf-8"))  # bytes?
        self.transport.close()


async def main():
    loop = asyncio.get_running_loop()
    server = await loop.create_server(
        lambda: UpcaseServerProtocol(),
        port=44444,
        reuse_port=True,
        reuse_address=True,
    )
    async with server:
        await server.serve_forever()


asyncio.run(main(), debug=True)

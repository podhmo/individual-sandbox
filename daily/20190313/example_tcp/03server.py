import logging
import sys
import asyncio


async def handle(reader, writer):
    data = (await reader.read(4096)).strip()
    message = data.decode()
    addr = writer.get_extra_info("peername")
    print(f"Received {message!r} from {addr!r}", file=sys.stderr)

    out_message = message.upper()
    print(f"Send: {out_message!r}", file=sys.stderr)
    writer.write(out_message.encode("utf-8"))
    await writer.drain()

    writer.close()


async def main():
    server = await asyncio.start_server(
        handle, "127.0.0.1", 44444, reuse_port=True, reuse_address=True
    )

    addr = server.sockets[0].getsockname()
    print(f"Serving on {addr}")

    async with server:
        await server.serve_forever()


logging.basicConfig(level=logging.DEBUG)
asyncio.run(main(), debug=True)

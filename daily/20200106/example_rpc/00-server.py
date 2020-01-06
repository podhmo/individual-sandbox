import asyncio

from asyncrpc.server import UniCastServer


class Test:
    async def echo(self, msg):
        await asyncio.sleep(0.1)
        return msg


if __name__ == "__main__":
    server = UniCastServer(obj=Test(), ip_addrs="127.0.0.1", port=9001)

    loop = asyncio.get_event_loop()
    loop.run_until_complete(server.start())
    loop.run_forever()

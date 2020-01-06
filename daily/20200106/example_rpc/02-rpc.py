import asyncio
import aiozmq.rpc


class ServerHandler(aiozmq.rpc.AttrHandler):
    @aiozmq.rpc.method
    def remote_func(self, a: int, b: int) -> int:
        return a + b


async def go():
    server = await aiozmq.rpc.serve_rpc(ServerHandler(), bind="tcp://127.0.0.1:5555")
    client = await aiozmq.rpc.connect_rpc(connect="tcp://127.0.0.1:5555")
    print("->", "remote_func(1, 2)")
    ret = await client.call.remote_func(1, 2)
    assert 3 == ret
    print("<-", ret)
    server.close()
    client.close()


asyncio.run(go(), debug=True)

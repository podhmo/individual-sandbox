import asyncio
from asyncrpc.client import UniCastClient

# https://pythonhosted.org/asyncrpc/

if __name__ == "__main__":
    client = UniCastClient(interfaces_info=[("127.0.0.1", 9001)])
    loop = asyncio.get_event_loop()
    print(loop.run_until_complete(client.echo("test asyncrpc")))
    loop.run_until_complete(client.close())

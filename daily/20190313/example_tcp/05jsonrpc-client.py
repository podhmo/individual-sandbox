import asyncio
from pyls_jsonrpc.streams import JsonRpcStreamWriter


class WriterAdapter:
    def __init__(self, writer: asyncio.StreamWriter):
        self.writer = writer

    def __getattr__(self, name):
        return getattr(self.writer, name)

    def flush(self):
        pass  # xxx:

    @property
    def closed(self):
        return self.writer.is_closing()  # xxx


async def tcp_echo_client(message):
    reader, writer = await asyncio.open_connection("127.0.0.1", 44444)

    w = JsonRpcStreamWriter(WriterAdapter(writer), indent=2)
    w.write(message)

    data = await reader.read(4096)
    print(f"Received: {data.decode()!r}")

    print("Close the connection")
    writer.close()


asyncio.run(tcp_echo_client("Hello World!"))

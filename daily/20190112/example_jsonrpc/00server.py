from jsonrpc.dispatchers import MethodDispatcher
from jsonrpc.endpoint import Endpoint
from jsonrpc.streams import JsonRpcStreamReader, JsonRpcStreamWriter

MAX_WORKERS = 64


class Context:
    def __init__(self, dispatcher, reader, writer):
        self.reader = JsonRpcStreamReader(reader)
        self.writer = JsonRpcStreamWriter(writer)

        write = self.writer.write
        self.endpoint = Endpoint(dispatcher, write, max_workers=MAX_WORKERS)

    def shutdown(self):
        self.endpoint.shutdown()
        self.reader.close()
        self.writer.close()


class Server(MethodDispatcher):
    def __init__(self, reader, writer):
        self._context = Context(self, reader, writer)
        self._shutdown = False

    def m_ping(self, **kwargs):
        self._context.writer.write("ping")

    def m_exit(self, **_kwargs):
        self._context.shutdown()

    def start(self):
        self._context.reader.listen(self._context.endpoint.consume)


if __name__ == "__main__":
    import sys
    stdin = sys.stdin.buffer
    stdout = sys.stdout.buffer
    s = Server(stdin, stdout)
    s.start()

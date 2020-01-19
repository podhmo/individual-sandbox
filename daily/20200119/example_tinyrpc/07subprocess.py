import typing as t
import logging
import sys
import subprocess
from functools import partial
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.server import RPCServer
from tinyrpc.client import RPCClient
from tinyrpc.transports import ClientTransport, ServerTransport
import handofcats

logger = logging.getLogger(__name__)


dispatcher = RPCDispatcher()


@dispatcher.public
def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    import random

    if random.random() > 0.5:
        raise Exception("oops")
    return {"q": f"{x} + {y} = ?", "a": ans}


def send(body: bytes, *, port: t.IO[bytes]):
    size = len(body)

    # print(size, file=port)
    port.write(str(size).encode("utf-8"))
    port.write(b"\n")

    # print(body, file=port, end="")
    port.write(body)

    # logger.debug("send	size:%d	body:%r", size, body)
    port.flush()


def recv(*, port: t.IO[bytes]) -> bytes:
    size = port.readline()
    body = port.read(int(size))
    # logger.debug("recv	size:%s	body:%r", size, body)
    return body


class SubprocessClientTransport(ClientTransport):
    def __init__(self, *, input_port: t.IO[bytes], output_port: t.IO[bytes]) -> None:
        self.input_port = input_port
        self.output_port = output_port

    def send_message(self, message: bytes, exact_reply: bool = True) -> bytes:
        send(message, port=self.output_port)
        return recv(port=self.input_port)


class SubprocessServerTransport(ServerTransport):
    def __init__(self, *, input_port: t.IO[bytes], output_port: t.IO[bytes]) -> None:
        self.input_port = input_port
        self.output_port = output_port

    def receive_message(self) -> t.Tuple[t.Any, bytes]:
        context = "*context*"
        return context, recv(port=self.input_port)

    def send_reply(self, context: t.Any, reply: bytes):
        print(context)
        send(reply, port=self.output_port)


@handofcats.as_subcommand
def manager():
    cmd = [sys.executable, __file__, "worker"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=False)

    rpc_client = RPCClient(
        JSONRPCProtocol(),
        SubprocessClientTransport(input_port=p.stdout, output_port=p.stdin),
    )
    for i in range(3):
        proxy = rpc_client.get_proxy()
        try:
            result = proxy.add(10, 20)
            print(f"result is {result}")
        except Exception as e:
            logger.error("!! %r", e, exc_info=True)
    p.terminate()


@handofcats.as_subcommand
def worker():
    rpc_server = RPCServer(
        SubprocessServerTransport(
            input_port=sys.stdin.buffer, output_port=sys.stdout.buffer
        ),
        JSONRPCProtocol(),
        dispatcher,
    )
    rpc_server.trace = partial(print, file=sys.stderr)
    rpc_server.serve_forever()


handofcats.as_subcommand.run()

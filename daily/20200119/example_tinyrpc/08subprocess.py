import typing as t
import sys
import subprocess
import threading
import logging
from functools import partial

import zmq
import handofcats

from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.transports.zmq import ZmqServerTransport
from tinyrpc.transports.zmq import ZmqClientTransport
from tinyrpc.server import RPCServer
from tinyrpc.client import RPCClient

logger = logging.getLogger(__name__)
dispatcher = RPCDispatcher()


@dispatcher.public
def post(msg: str):
    print(f"\x1b[32m{msg!r}\x1b[0m")


def send(body: bytes, *, port: t.IO[bytes]):
    size = len(body)

    print(size, file=port)
    print(body, file=port, end="")

    logger.debug("send	size:%d	body:%r", size, body)
    port.flush()


def recv(*, port: t.IO[str]) -> str:
    size = port.readline()
    if not size:
        return ""
    body = port.read(int(size))
    logger.debug("recv	size:%s	body:%r", size, body)
    return body


@handofcats.as_subcommand
def manager():
    ctx = zmq.Context()
    transport = ZmqServerTransport.create(ctx, "tcp://127.0.0.1:5002")

    rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)
    # rpc_server.trace = print
    th = threading.Thread(daemon=True, target=rpc_server.serve_forever)
    th.start()

    cmd = [sys.executable, __file__, "worker"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, text=True)
    for i in range(10):
        send(f"world{i}", port=p.stdin)
    p.stdin.close()

    th.join()
    p.terminate()


@handofcats.as_subcommand
def worker():
    ctx = zmq.Context()
    rpc_client = RPCClient(
        JSONRPCProtocol(), ZmqClientTransport.create(ctx, "tcp://127.0.0.1:5002")
    )
    proxy = rpc_client.get_proxy()

    for message in iter(partial(recv, port=sys.stdin), ""):
        proxy.post(f"hello {message}")
        import time

        time.sleep(0.4)
    sys.stdout.write("")
    sys.stdout.close()


handofcats.as_subcommand.run()

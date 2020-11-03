import sys
import pathlib
import time
import zmq
from tinyrpc.server import RPCServer
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.transports.zmq import ZmqServerTransport
from handofcats import as_command

ctx = zmq.Context()
dispatcher = RPCDispatcher()


@dispatcher.public(name="add")
def adder_function(x, y):
    return x + y


dispatcher.public(pow)


@dispatcher.public
def mul(x, y):
    return x * y


@dispatcher.public
def list_methods():
    return list(dispatcher.method_map.keys())


@as_command
def run(*, port: int = 8888, sentinel: str = "") -> None:
    if sentinel:
        p = pathlib.Path(sentinel)
        ok = False
        for wait_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if not p.exists():
                print(f"	wait ... {wait_time}", file=sys.stderr)
                time.sleep(wait_time)
                continue

            print(f"ack {sentinel}", file=sys.stderr)
            p.unlink()
            ok = True
            break
        if not ok:
            raise RuntimeError(f"timeout, f{sentinel}, {time.time()-st}")

    print(f"listen ... {port}", file=sys.stderr)
    transport = ZmqServerTransport.create(ctx, f"tcp://127.0.0.1:{port}")
    rpc_server = RPCServer(transport, JSONRPCProtocol(), dispatcher)
    rpc_server.serve_forever()

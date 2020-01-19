import typing as t
from tinyrpc.protocols.jsonrpc import JSONRPCProtocol
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.server import RPCServer
from tinyrpc.client import RPCClient
from tinyrpc.transports import ClientTransport, ServerTransport


dispatcher = RPCDispatcher()


@dispatcher.public
def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    import random

    if random.random() > 0.5:
        raise Exception("oops")
    return {"q": f"{x} + {y} = ?", "a": ans}


in_box: bytes = []
out_box: bytes = []


class CallbackClientTransport(ClientTransport):
    def send_message(self, message: bytes, exact_reply: bool = True) -> bytes:
        in_box.append(message)
        rpc_server.receive_one_message()  # 代わりにsend時に直接呼ぶ
        return out_box.pop(0)


class CallbackServerTransport(ServerTransport):
    def receive_message(self) -> t.Tuple[t.Any, bytes]:
        context = "*context*"
        return context, in_box.pop(0)

    def send_reply(self, context: t.Any, reply: bytes):
        print(context)
        out_box.append(reply)


rpc_server = RPCServer(CallbackServerTransport(), JSONRPCProtocol(), dispatcher,)
rpc_server.trace = print

# 本来はこれを実行する
# rpc_server.serve_forever()

rpc_client = RPCClient(JSONRPCProtocol(), CallbackClientTransport())
proxy = rpc_client.get_proxy()

try:
    result = proxy.add(10, 20)
    print(f"result is {result}")
except Exception as e:
    print(e)

from tinyrpc.protocols import jsonrpc
from tinyrpc.dispatch import RPCDispatcher

p = jsonrpc.JSONRPCProtocol()
dispatcher = RPCDispatcher()


@dispatcher.public
def do_task(msg: str) -> None:
    print(f"do, {msg}")


# method, args, kwargs, one_way
req = p.create_request("do_task", ["hello"], kwargs={})
res = dispatcher.dispatch(req)

print(req.serialize())
print(res.serialize())

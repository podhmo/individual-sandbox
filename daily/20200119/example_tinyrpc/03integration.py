import typing as t
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.protocols import jsonrpc

protocol = jsonrpc.JSONRPCProtocol()
dispatcher = RPCDispatcher()


@dispatcher.public
def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    return {"q": f"{x} + {y} = ?", "a": ans}


req = protocol.create_request("add", args=[10, 20], kwargs={})
"->", req.serialize()  # => ('->', b'{"jsonrpc": "2.0", "method": "add", "params": [10, 20], "id": 1}')
res = dispatcher.dispatch(req)
"<-", res.serialize()  # => ('<-', b'{"jsonrpc": "2.0", "id": 1, "result": {"q": "10 + 20 = ?", "a": 30}}')

req = protocol.create_request("add", args=[], kwargs={"x": 10, "y": 20})
"->", req.serialize()  # => ('->', b'{"jsonrpc": "2.0", "method": "add", "params": {"x": 10, "y": 20}, "id": 2}')
res = dispatcher.dispatch(req)
"<-", res.serialize()  # => ('<-', b'{"jsonrpc": "2.0", "id": 2, "result": {"q": "10 + 20 = ?", "a": 30}}')

dispatcher.validator = None
req = protocol.create_request("add", args=[10, 20])
"->", req.serialize()  # => ('->', b'{"jsonrpc": "2.0", "method": "add", "params": [10, 20], "id": 3}')
res = dispatcher.dispatch(req)
"<-", res.serialize()  # => ('<-', b'{"jsonrpc": "2.0", "id": 3, "error": {"message": "add() argument after ** must be a mapping, not NoneType", "code": -32000}}')

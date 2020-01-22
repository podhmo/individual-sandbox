from tinyrpc.protocols import jsonrpc

p = jsonrpc.JSONRPCProtocol()


def add(x: int, y: int) -> int:
    return x + y


# method, args, kwargs, one_way
req = p.create_request("add", [10, 20], kwargs={})

res = req.respond(30)
print(req.serialize())
print(res.serialize())

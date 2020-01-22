from tinyrpc.protocols import jsonrpc

p = jsonrpc.JSONRPCProtocol()


def do_task(msg: str) -> None:
    print(f"do, {msg}")


# method, args, kwargs, one_way
req = p.create_request("do_task", ["hello"], kwargs={})

res = req.respond(30)
print(req.serialize())
print(req.one_way)

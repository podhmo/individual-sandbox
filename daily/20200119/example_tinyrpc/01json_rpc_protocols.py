from tinyrpc.protocols import jsonrpc

p = jsonrpc.JSONRPCProtocol()


def add(x: int, y: int) -> int:
    return x + y


# method, args, kwargs, one_way
req = p.create_request("add", [10, 20])

req  # => <tinyrpc.protocols.jsonrpc.JSONRPCRequest object at 0x103236ac0>
req.serialize()  # => b'{"jsonrpc": "2.0", "method": "add", "params": [10, 20], "id": 1}'

p.parse_request(
    req.serialize()
).serialize()  # => b'{"jsonrpc": "2.0", "method": "add", "params": [10, 20], "id": 1}'


res = req.respond(30)

res  # => <tinyrpc.protocols.jsonrpc.JSONRPCSuccessResponse object at 0x103236b50>
res.serialize()  # => b'{"jsonrpc": "2.0", "id": 1, "result": 30}'


print(req.serialize())
print(res.serialize())
# -- stdout --------------------
# >> b'{"jsonrpc": "2.0", "method": "add", "params": [10, 20], "id": 1}'
# >> b'{"jsonrpc": "2.0", "id": 1, "result": 30}'

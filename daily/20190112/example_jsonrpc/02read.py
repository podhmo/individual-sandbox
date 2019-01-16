from io import BytesIO
from pyls_jsonrpc.streams import JsonRpcStreamReader

lines = [
    b'Content-Length: 49\r\n'
    b'Content-Type: application/vscode-jsonrpc; charset=utf8\r\n'
    b'\r\n'
    b'{"id": "hello", "method": "method", "params": {}}'
]

o = BytesIO()
for line in lines:
    o.write(line)
o.seek(0)

r = JsonRpcStreamReader(o)


def consume(d):
    print("!", d, "!")


r.listen(consume)
r.close()

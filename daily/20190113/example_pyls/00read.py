import json
from io import BytesIO
from pyls_jsonrpc.streams import JsonRpcStreamReader
o = BytesIO()
r = JsonRpcStreamReader(o)

message = {
    "jsonrpc": "2.0",
    "method": "textDocument/codeLens",
    "params": {
        "uri": "file://00size.py",
    },
    "id": 1,
}
body = json.dumps(message).encode("utf-8")
lines = [
    f"Content-Length: {len(body)}\r\n".encode("utf-8"),
    b"\r\n",
    body,
]
for line in lines:
    o.write(line)
o.seek(0)
print(r._read_message())

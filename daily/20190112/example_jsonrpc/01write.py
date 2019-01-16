import sys
from pyls_jsonrpc.streams import JsonRpcStreamWriter

w = JsonRpcStreamWriter(sys.stdout.buffer)
w.write({"key": "value"})
w.close()
# Content-Length: 16
# Content-Type: application/vscode-jsonrpc; charset=utf8

# {"key": "value"}

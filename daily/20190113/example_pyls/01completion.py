import json
import sys
from io import BytesIO
from pyls.python_ls import (
    PythonLanguageServer,
)
o = BytesIO()


def request(message, o=o):
    o.seek(0)
    body = json.dumps(message).encode("utf-8")
    lines = [f"Content-Length: {len(body)}".encode("utf-8"), b"\r\n", b"\r\n", body]
    for line in lines:
        o.write(line)
    o.seek(0)


import logging
logging.basicConfig(level=logging.DEBUG)
# logging.basicConfig(level=logging.INFO)
check_parent_process = False
server = PythonLanguageServer(o, sys.stdout.buffer, check_parent_process)


# t = threading.Thread(target=server.start, daemon=True)
# t.start()

import os
message = {
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
        "processId": os.getpid(),
        "rootUri": f"file://{os.getcwd()}",
        "rootPath": os.getcwd(),
    },
    "id": 1,
}

request(message)
server.start()

message = {
    "jsonrpc": "2.0",
    "method": "textDocument/completion",
    "params": {
        "textDocument": {
            "uri": f"file://{os.getcwd()}/something.py",
        },
        "position": {
            "line": 1,
            "character": 7,
        },
    },
    "id": 2,
}

request(message)
server.start()
server.m_shutdown()
server.m_exit()

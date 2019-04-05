# server: pyls --tcp --port 44444 -vv
import pathlib
import socket
import uuid
import threading
from pprint import pprint
from pyls_jsonrpc.streams import JsonRpcStreamWriter, JsonRpcStreamReader

host = "localhost"
port = 44444


with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
    sock.connect((host, port))

    def consume():
        JsonRpcStreamReader(sock.makefile("rb")).listen(pprint)

    th = threading.Thread(target=consume, daemon=True)
    th.start()
    w = JsonRpcStreamWriter(sock.makefile("wb"), indent=2)

    d = {
        "jsonrpc": "2.0",
        "id": str(uuid.uuid4()),
        "method": "initialize",
        "params": {
            "rootUri": pathlib.Path(__file__).parent.absolute().as_uri(),
            "initializationOptions": {},  # xxx:
        },
    }
    w.write(d)

    d = {
        "jsonrpc": "2.0",
        "id": str(uuid.uuid4()),
        "method": "textDocument/completion",
        "params": {
            "textDocument": {
                "uri": pathlib.Path(__file__)
                .parent.joinpath("something.py")
                .absolute()
                .as_uri()
            },
            "position": {"line": 1, "character": 7},
        },
    }
    from dictknife import loading
    loading.dumpfile(d, format="json")
    w.write(d)
    th.join(2)

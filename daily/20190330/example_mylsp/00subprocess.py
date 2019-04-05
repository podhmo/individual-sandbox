import os.path
import subprocess
import uuid
import threading
import pathlib
import logging
from pprint import pprint
from pyls_jsonrpc.streams import JsonRpcStreamWriter, JsonRpcStreamReader


def make_request(method, params):
    d = {"jsonrpc": "2.0", "id": str(uuid.uuid4())}
    d["method"] = method
    print("!!", d)
    d.update(params)
    return d


logging.basicConfig(level=logging.DEBUG)
args = ["pyls"]
with subprocess.Popen(args, stdout=subprocess.PIPE, stdin=subprocess.PIPE) as p:

    def consume():
        JsonRpcStreamReader(p.stdout).listen(pprint)

    th = threading.Thread(target=consume, daemon=True)
    th.start()

    w = JsonRpcStreamWriter(p.stdin, indent=2)
    d = make_request(
        "initialize",
        {
            "params": {
                "rootPath": os.path.dirname(__file__) or os.getcwd(),  # hmm
                "initializationOptions": {},  # xxx:
                "trace": "verbose",  # off | messages | verbose
            }
        },
    )
    w.write(d)

    d = make_request(
        "textDocument/definition",
        {
            "params": {
                "textDocument": {
                    "uri": pathlib.Path(__file__)
                    .parent.absolute()
                    .joinpath("something.py")
                    .as_uri(),
                    "position": {"line": 1, "character": 1},
                }
            }
        },
    )
    w.write(d)

    d = make_request("shutdown", {})
    w.write(d)
    d = make_request("exit", {})
    w.write(d)

    try:
        p.wait(timeout=3)
    except subprocess.TimeoutExpired as e:
        print("!", e)
        p.kill()

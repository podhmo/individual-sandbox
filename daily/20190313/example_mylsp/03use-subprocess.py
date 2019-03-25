import subprocess
import uuid
import threading
import os.path
import logging
from pprint import pprint
from pyls_jsonrpc.streams import JsonRpcStreamWriter, JsonRpcStreamReader


logging.basicConfig(level=logging.DEBUG)
args = ["pyls"]
args = ["bingo", "-trace"]
with subprocess.Popen(args, stdout=subprocess.PIPE, stdin=subprocess.PIPE) as p:

    def consume():
        JsonRpcStreamReader(p.stdout).listen(pprint)

    th = threading.Thread(target=consume, daemon=True)
    th.start()

    w = JsonRpcStreamWriter(p.stdin, indent=2)
    d = {
        "jsonrpc": "2.0",
        "id": str(uuid.uuid4()),
        "method": "initialize",
        "params": {
            "rootPath": os.path.dirname(__file__) or os.getcwd(),  # hmm
            "initializationOptions": {},  # xxx:
            "trace": "verbose",  # off | messages | verbose
        },
    }
    w.write(d)
    d = {"jsonrpc": "2.0", "id": str(uuid.uuid4()), "method": "shutdown"}
    w.write(d)
    d = {"jsonrpc": "2.0", "id": str(uuid.uuid4()), "method": "exit"}
    w.write(d)
    try:
        p.wait(timeout=3)
    except subprocess.TimeoutExpired as e:
        print("!", e)
        p.kill()

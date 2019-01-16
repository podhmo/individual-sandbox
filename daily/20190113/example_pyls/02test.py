import json
import os
import unittest
from io import BytesIO
from pyls.python_ls import (
    PythonLanguageServer,
)


class _Client:
    def __init__(self, o):
        self.id = 0
        self.o = o

    def request(self, message):
        self.o.seek(0)
        body = json.dumps(message).encode("utf-8")
        lines = [
            f"Content-Length: {len(body)}".encode("utf-8"),
            b"\r\n",
            b"\r\n",
            body,
        ]
        for line in lines:
            self.o.write(line)
        self.o.seek(0)

    def make_message(self, method, params):
        self.id += 1
        return {
            "id": self.id,
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }


# suppress annoyed preload(HACK)
import pyls.plugins
import sys
sys.modules["pyls.plugins.preload_imports"] = object()


class Tests(unittest.TestCase):
    def test_it(self):
        inp = BytesIO()
        outp = BytesIO()
        client = _Client(inp)
        check_parent_process = True
        server = PythonLanguageServer(inp, outp, check_parent_process)

        # init
        params = {
            "processId": os.getpid(),
            "rootUri": f"file://{os.getcwd()}",
            "rootPath": os.getcwd(),
        }
        client.request(client.make_message("initialize", params))
        server.start()
        print(outp.getvalue().decode("utf-8"))

        # completion
        params = {
            "textDocument": {
                "uri": f"file://{os.getcwd()}/something.py",
            },
            "position": {
                "line": 1,
                "character": 7,
            },
        }
        client.request(client.make_message("textDocument/completion", params))
        server.start()
        print(outp.getvalue().decode("utf-8"))

        # shutdown
        params = {}
        client.request(client.make_message("shutdown", params))
        server.start()
        client.request(client.make_message("exit", params))
        print(outp.getvalue().decode("utf-8"))
        server.start()

        # xxx
        from jedi.evaluate.compiled.subprocess import _subprocesses
        for p in list(_subprocesses.values()):
            p.kill()


if __name__ == "__main__":
    import logging
    logging.basicConfig(level=logging.DEBUG)
    unittest.main()

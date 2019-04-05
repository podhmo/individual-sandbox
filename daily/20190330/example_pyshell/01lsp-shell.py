import uuid
import logging
import pathlib
import subprocess
import threading
import signal
from pprint import pprint
from cmd import Cmd
from dictknife.mkdict import mkdict

from pyls_jsonrpc.streams import JsonRpcStreamWriter, JsonRpcStreamReader


def make_request(extra):
    d = {"jsonrpc": "2.0", "id": str(uuid.uuid4())}
    d.update(extra)
    pprint(d)
    return d


# need pyls
class Client:
    def __init__(self, args):
        self.p = subprocess.Popen(args, stdout=subprocess.PIPE, stdin=subprocess.PIPE)

    def start(self):
        def consume():
            JsonRpcStreamReader(self.p.stdout).listen(pprint)

        th = threading.Thread(target=consume, daemon=True)
        th.start()
        return th

    def close(self):
        try:
            self.p.wait(1)
        except subprocess.TimeoutExpired:
            self.p.terminate()
            try:
                self.p.wait(1)
            except subprocess.TimeoutExpired:
                self.p.kill()

    def send(self, d):
        w = JsonRpcStreamWriter(self.p.stdin, indent=2)
        w.write(make_request(d))


class Prompt(Cmd):
    prompt = "> "

    def __init__(self, client: Client, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.d = {}
        self.client = client
        self.th = client.start()
        self.client.send(
            {
                "method": "initialize",
                "params": {
                    "rootUri": pathlib.Path(__file__).parent.absolute().as_uri(),
                    "initializationOptions": {},
                    "trace": "verbose",
                },
            }
        )

    def do_exit(self, inp):
        self.client.send({"method": "shutdown"})
        self.client.send({"method": "exit"})
        self.client.close()
        self.th.join(3)  # timeout
        return True

    do_INT = do_EOF = do_exit

    def do_send(self, line):
        d = mkdict(line, shared=self.d)
        if not d:
            return
        import json
        print("@", json.dumps(d))
        self.client.send(d)

    def default(self, line):
        if line.strip():
            return

        return self.do_send(line)

    def completedefault(self, *ignores):
        return ["method", "params", "textDocument/uri", "position", "line", "character"]


logging.basicConfig(level=logging.DEBUG)


client = Client(["pyls", "-vv"])
prompt = Prompt(client)
prompt.cmdloop()

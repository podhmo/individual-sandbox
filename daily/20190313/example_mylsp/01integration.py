import os
from threading import Thread
import logging

from pyls.python_ls import start_io_lang_server, PythonLanguageServer

CALL_TIMEOUT = 2


def start_client(client):
    client.start()


class _ClientServer(object):
    """ A class to setup a client/server pair """

    def __init__(self, check_parent_process=False):
        # Client to Server pipe
        csr, csw = os.pipe()
        # Server to client pipe
        scr, scw = os.pipe()

        self.server_thread = Thread(
            target=start_io_lang_server,
            args=(
                os.fdopen(csr, "rb"),
                os.fdopen(scw, "wb"),
                check_parent_process,
                PythonLanguageServer,
            ),
        )
        self.server_thread.daemon = True
        self.server_thread.start()

        self.client = PythonLanguageServer(
            os.fdopen(scr, "rb"), os.fdopen(csw, "wb"), start_io_lang_server
        )
        self.client_thread = Thread(target=start_client, args=[self.client])
        self.client_thread.daemon = True
        self.client_thread.start()


logging.basicConfig(level=logging.DEBUG)
client_server_pair = _ClientServer()
client_server = client_server_pair.client
response = client_server._endpoint.request(
    "initialize", {"rootPath": os.path.dirname(__file__), "initializationOptions": {}}
).result(timeout=CALL_TIMEOUT)
assert "capabilities" in response

shutdown_response = client_server_pair.client._endpoint.request("shutdown").result(
    timeout=CALL_TIMEOUT
)
assert shutdown_response is None
client_server_pair.client._endpoint.notify("exit")

import logging
from collections import ChainMap
from pyls_jsonrpc.dispatchers import MethodDispatcher
from pyls_jsonrpc.endpoint import Endpoint

logging.basicConfig(level=logging.DEBUG)


class Dispatcher(MethodDispatcher):
    def m_add(self, *, x: int, y: int) -> int:
        return x + y


def consume(*args, **kwargs):
    print("!", args, kwargs)


endpoint = Endpoint(Dispatcher(), consume)
message = {
    "jsonrpc": "2.0",
    "method": "add",
    "params": {
        "x": 10,
        "y": 20
    },
}

print("-")
endpoint.consume(message)
print("-")
endpoint.consume(ChainMap({"id": 1}, message))

print("-")
endpoint.consume(ChainMap({"method": "foo", "id": 2}, message))

endpoint.shutdown()
# -
# DEBUG:pyls_jsonrpc.endpoint:Handling notification from client {'jsonrpc': '2.0', 'method': 'add', 'params': {'x': 10, 'y': 20}}
# -
# DEBUG:pyls_jsonrpc.endpoint:Handling request from client ChainMap({'id': 1}, {'jsonrpc': '2.0', 'method': 'add', 'params': {'x': 10, 'y': 20}})
# DEBUG:pyls_jsonrpc.endpoint:Got result from synchronous request handler: 30
# ! ({'jsonrpc': '2.0', 'id': 1, 'result': 30},) {}
# -
# DEBUG:pyls_jsonrpc.endpoint:Handling request from client ChainMap({'method': 'foo', 'id': 2}, {'jsonrpc': '2.0', 'method': 'add', 'params': {'x': 10, 'y': 20}})
# ERROR:pyls_jsonrpc.endpoint:Failed to handle request 2
# Traceback (most recent call last):
#   File "VENV/lib/python3.7/site-packages/pyls_jsonrpc/endpoint.py", line 178, in _handle_request
#     handler = self._dispatcher[method]
#   File "VENV/lib/python3.7/site-packages/pyls_jsonrpc/dispatchers.py", line 26, in __getitem__
#     raise KeyError()
# KeyError

# During handling of the above exception, another exception occurred:

# Traceback (most recent call last):
#   File "VENV/lib/python3.7/site-packages/pyls_jsonrpc/endpoint.py", line 113, in consume
#     self._handle_request(message['id'], message['method'], message.get('params'))
#   File "VENV/lib/python3.7/site-packages/pyls_jsonrpc/endpoint.py", line 180, in _handle_request
#     raise JsonRpcMethodNotFound.of(method)
# pyls_jsonrpc.exceptions.JsonRpcMethodNotFound: Method Not Found: foo
# ! ({'jsonrpc': '2.0', 'id': 2, 'error': {'code': -32601, 'message': 'Method Not Found: foo'}},) {}

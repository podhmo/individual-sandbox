import logging
from queue import Queue
from pyls_jsonrpc.dispatchers import MethodDispatcher
from pyls_jsonrpc.endpoint import Endpoint

logging.basicConfig(level=logging.DEBUG)


class Dispatcher(MethodDispatcher):
    def m_add(self, *, x: int, y: int) -> int:
        print("add!", x, y, x + y)
        return x + y


q = Queue()


def consume(params: dict) -> None:
    global q
    q.put_nowait(params)


endpoint = Endpoint(Dispatcher(), consume)
# notifyは単にconsumerに渡すだけ
endpoint.notify("add", params={"x": 10, "y": 20})

# requestはfuturesのmappingに格納して終わりっぽい？
fut = endpoint.request("add", params={"x": 10, "y": 20})
while not q.empty():
    message = q.get()
    if "id" in message:
        result = endpoint._dispatcher[message["method"]](message["params"])
        endpoint._handle_response(message["id"], result=result)

endpoint.shutdown()

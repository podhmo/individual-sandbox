import logging
import typing as t
from collections import ChainMap
from pyls_jsonrpc.dispatchers import MethodDispatcher
from pyls_jsonrpc.endpoint import Endpoint


class Dispatcher(MethodDispatcher):
    def m_hello(self, *, name: str) -> t.Callable[[], t.Dict]:
        import time

        print(f"*** {name}: hello ***")
        t = time.time()

        def cont() -> dict:

            time.sleep(1)
            print(f"*** {name}: bye ***")
            return {"time": time.time() - t}

        return cont


def consume(*args, **kwargs):
    print("!!!", args, kwargs)


logging.basicConfig(
    level=logging.DEBUG,
    format="\t" + logging.BASIC_FORMAT,
)
endpoint = Endpoint(Dispatcher(), consume)
message = {
    "jsonrpc": "2.0",
    "method": "hello",
    "params": {
        "name": "foo",
    },
}

endpoint.consume(ChainMap({"id": 1}, message))
endpoint.consume(ChainMap({"id": 2, "params": {"name": "bar"}}, message))
endpoint.shutdown()

from pyls_jsonrpc.dispatchers import MethodDispatcher


class Dispatcher(MethodDispatcher):
    def m_add(self, *, x: int, y: int) -> int:
        return x + y


d = Dispatcher()
print(d["add"]({"x": 10, "y": 20}))

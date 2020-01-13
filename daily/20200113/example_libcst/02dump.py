import libcst
import inspect
import hello


class DumpVisitor(libcst.CSTVisitor):
    def __init__(self):
        self.stack = []

    def on_visit(self, node: libcst.CSTNode) -> None:
        print(f"{' ' * len(self.stack)}V: {type(node)!r}")
        self.stack.append(node)
        super().on_visit(node)

    def on_leave(self, node: libcst.CSTNode) -> None:
        super().on_leave(node)
        assert node is self.stack.pop()
        print(f"{' ' * len(self.stack)}L: {type(node)!r}")


code = inspect.getsource(hello)

t = libcst.parse_module(code)
v = DumpVisitor()
t.visit(v)

# print(type(t))
# print(t)
